module HellSmack.ModLoader.Forge
  ( ForgeVersion (..),
    isPre113,
    isJigsaw,
    allVersionsManifestPath,
    findVersion,
    getVersionManifest,
    launch,
  )
where

import Codec.Archive.Zip
import Control.Lens.Unsound (adjoin)
import Data.Conduit.Process.Typed
import Data.List (stripPrefix)
import Data.List.Lens
import Data.List.Split (splitOn)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Lens
import Data.Time
import HellSmack.Logging
import HellSmack.ModLoader
import HellSmack.Util
import HellSmack.Util.Meta qualified as Meta
import HellSmack.Vanilla qualified as V
import HellSmack.Yggdrasil
import System.IO.Temp
import UnliftIO.IO

-- $setup
-- >>> import "hellsmack" Prelude
-- >>> import Test.Tasty.HUnit

newtype ForgeVersion = ForgeVersion {unForgeVersion :: Text}
  deriving stock (Generic)
  deriving newtype (Ord, Eq, FromJSON)
  deriving (Show) via (ShowWithoutQuotes Text)

-- $
-- >>> isPre113 (ForgeVersion "1.16.4-35.1.28")
-- False
-- >>> isPre113 (ForgeVersion "1.15-29.0.1")
-- False
-- >>> isPre113 (ForgeVersion "1.12.1-14.22.1.2480")
-- True
-- >>> isPre113 (ForgeVersion "1.4.5-6.4.2.447")
-- True
-- >>> isPre113 (ForgeVersion "1.7.10-10.13.2.1352-1.7.10")
-- True

mcMajorMinor :: MonadFail m => ForgeVersion -> m (Int, Int)
mcMajorMinor (ForgeVersion v) = case v
  & T.takeWhile (/= '-')
  & T.splitOn "."
  & take 2
  & each %%~ readMaybe . toString of
  Just [maj, min] -> pure (maj, min)
  _ -> fail [i|invalid forge version format: $v|]

isPre113 :: MonadFail m => ForgeVersion -> m Bool
isPre113 = fmap (< (1, 13)) . mcMajorMinor

isJigsaw :: MonadFail m => ForgeVersion -> m Bool
isJigsaw = fmap (>= (1, 17)) . mcMajorMinor

data VersionManifest = VersionManifest
  { id :: ForgeVersion,
    inheritsFrom :: V.MCVersion,
    time :: UTCTime,
    releaseTime :: UTCTime,
    versionType :: V.VersionType,
    arguments :: Maybe V.Arguments,
    minecraftArguments :: Maybe Text,
    mainClass :: Text,
    libraries :: [Library]
  }
  deriving stock (Show, Generic)
  deriving (FromJSON) via CustomJSONLabel '[Rename "versionType" "type"] VersionManifest

data Library = Post113Lib V.Library | Pre113Lib Pre113Library
  deriving stock (Show, Generic)

instance FromJSON Library where
  parseJSON v = (Post113Lib <$> parseJSON v) <|> (Pre113Lib <$> parseJSON v)

data Pre113Library = Pre113Library
  { url :: Maybe Text,
    name :: MavenId,
    checksums :: Maybe [SHA1],
    natives :: Maybe (Map V.OSName Text),
    extract :: Maybe V.Extract,
    rules :: Maybe [V.Rule],
    serverreq :: Maybe Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data InstallerManifest = InstallerManifest
  { dataEntries :: Map Text DataEntry,
    processors :: [Processor],
    libraries :: [V.Library]
  }
  deriving stock (Show, Generic)
  deriving (FromJSON) via CustomJSONLabel '[Rename "dataEntries" "data"] InstallerManifest

data DataEntry = DataEntry
  { client :: Text,
    server :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data Processor = Processor
  { jar :: MavenId,
    classpath :: [MavenId],
    args :: [Text],
    outputs :: Maybe (Map Text Text),
    sides :: Maybe [MCSide]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

allVersionsManifestPath :: MRHas r DirConfig m => m (Path Abs File)
allVersionsManifestPath =
  siehs @DirConfig $ #manifestDir . to (</> [relfile|forgeVersionManifest.json|])

findVersion ::
  (MonadIO m, MRHasAll r [DirConfig, Manager] m) =>
  V.MCVersion ->
  VersionQuery ->
  m ForgeVersion
findVersion (coerce -> mcVersion) fvq = do
  manifestPath <- allVersionsManifestPath
  let promoOrVersion = case fvq of
        ConcreteVersion version -> Right version
        RecommendedVersion -> Left ("recommended" :: Text)
        LatestVersion -> Left "latest"
  (view chosen -> version) <-
    promoOrVersion & _Left %%~ \promoKey ->
      downloadJson promotionsUrl
        >>= rethrow . \(versions :: Value) ->
          versions ^? (key "promos" . key [i|$mcVersion-$promoKey|] . _String)
            & maybeToRight "no promoted version found"
  downloadMaybeJson manifestUrl manifestPath Nothing \(versions :: Value) ->
    case versions ^.. key mcVersion . values . _String . filtered (version `T.isInfixOf`) of
      [v] -> Right $ ForgeVersion v
      [] -> Left [i|invalid forge version: $version|]
      vs -> Left $ [i|multiple matching forge versions: ${}|] $ T.intercalate ", " vs
  where
    manifestUrl = "https://files.minecraftforge.net/maven/net/minecraftforge/forge/maven-metadata.json"
    promotionsUrl = "https://files.minecraftforge.net/maven/net/minecraftforge/forge/promotions_slim.json"

getVersionManifest ::
  (MonadIO m, MRHasAll r [DirConfig, Manager] m) =>
  ForgeVersion ->
  m VersionManifest
getVersionManifest fv = do
  isPre113 <- rethrow $ isPre113 fv
  let findVersionFile = do
        es <- mkEntrySelector "version.json"
        doesEntryExist es >>= \case
          True -> pure es
          False -> mkEntrySelector "install_profile.json"
  forgeFile <- reThrow $ parseRelFile [i|forge-${show fv}.json|]
  toPath <- siehs @DirConfig $ #manifestDir . to (</> forgeFile)
  json :: Value <- decodeJSON . toLazy =<< extractFromInstaller fv toPath findVersionFile getEntry
  vm <- decodeJSONValue case (json ^? key "install" . key "minecraft", json ^? key "versionInfo") of
    (Just mcVersion, Just viJson) -> viJson & _Object . at "inheritsFrom" ?~ mcVersion
    _ -> json

  let fixUrls =
        each %~ do
          ( #_Post113Lib . filterByAId "forge" %~ \l ->
              let mi = l ^. #name & #classifier ?~ if isPre113 then "universal" else "launcher"
               in l & #downloads . #artifact . _Just . #url .~ [i|$forgeMavenUrl/${}|] (mavenIdUrl mi)
            )
            . ( #_Pre113Lib
                  %~ (filterByAId "forge" %~ #name . #classifier ?~ "universal")
                    . ( filterByAId "minecraftforge" %~ do
                          #name %~ (#artifactId .~ "forge") . (#version .~ unForgeVersion fv)
                      )
              )
  pure $ vm & #libraries %~ fixUrls
  where
    filterByAId aid = filtered $ has $ #name . #artifactId . only aid

forgeInstallerMavenId :: ForgeVersion -> MavenId
forgeInstallerMavenId fv =
  MavenId
    { groupId = "net.minecraftforge",
      artifactId = "forge",
      version = coerce fv,
      classifier = Just "installer",
      extension = Nothing
    }

forgeInstallerPath :: (MonadThrow m, MRHas r DirConfig m) => ForgeVersion -> m (Path Abs File)
forgeInstallerPath = libraryPath . forgeInstallerMavenId

vanillaMavenUrl :: Text
vanillaMavenUrl = "https://libraries.minecraft.net"

forgeMavenUrl :: Text
forgeMavenUrl = "https://files.minecraftforge.net/maven"

downloadForgeInstaller :: (MonadIO m, MRHasAll r [DirConfig, Manager] m) => ForgeVersion -> m ()
downloadForgeInstaller fv = do
  let url = [i|$forgeMavenUrl/${}|] $ mavenIdUrl $ forgeInstallerMavenId fv
  path <- reThrow $ forgeInstallerPath fv
  downloadHash url path Nothing ShowProgress

extractFromInstaller ::
  (MonadIO m, MRHasAll r [DirConfig, Manager] m) =>
  ForgeVersion ->
  Path Abs File ->
  ZipArchive EntrySelector ->
  (EntrySelector -> ZipArchive a) ->
  m a
extractFromInstaller fv toPath getES action = do
  downloadForgeInstaller fv
  zipPath <- reThrow $ forgeInstallerPath fv
  ensureDir $ parent toPath
  withArchive (toFilePath zipPath) do
    es <- getES
    saveEntry es (toFilePath toPath)
    action es

extractFromInstaller' ::
  (MonadIO m, MRHasAll r [DirConfig, Manager] m) =>
  ForgeVersion ->
  Path Rel File ->
  Path Abs File ->
  m ()
extractFromInstaller' fv fromPath toPath =
  extractFromInstaller fv toPath (mkEntrySelector $ toFilePath fromPath) (pure pass)

getLibraries ::
  (MonadThrow m, MRHasAll r [DirConfig, MCSide] m) =>
  VersionManifest ->
  m [V.Library]
getLibraries vm = do
  side <- sieh @MCSide
  let useLib = \case
        Pre113Lib l ->
          side == MCClient
            || (l ^. #name . #artifactId) `elem` ["forge", "minecraftforge"]
            || l ^. #serverreq == Just True
        _ -> True
  forM (vm ^. #libraries & filter useLib) \case
    Pre113Lib Pre113Library {..} -> do
      let baseUrl = if isJust url then forgeMavenUrl else vanillaMavenUrl
          toArtifact name sha1 = do
            path <- mavenIdPath name
            let url = [i|$baseUrl/${}|] $ mavenIdUrl name
            pure V.Artifact {size = Nothing, ..}
      artifact <- Just <$> toArtifact name (checksums ^? _Just . _head)
      classifiers <-
        natives & _Just %%~ do
          fmap M.fromList
            . traverse ((_2 %%~ \c -> toArtifact (name & #classifier ?~ c) Nothing) . dup)
            . toList
      pure V.Library {downloads = V.LibraryDownload {..}, ..}
    Post113Lib l -> pure l

mergeWithVanilla :: V.VersionManifest -> VersionManifest -> [V.Library] -> V.VersionManifest
mergeWithVanilla vm VersionManifest {..} libs =
  vm
    & #id .~ V.MCVersion [i|forge-${show id}|]
    & #time .~ time
    & #releaseTime .~ releaseTime
    & #versionType .~ versionType
    & #mainClass .~ mainClass
    & #libraries %~ (libs <>)
    & #minecraftArguments .~ minecraftArguments
    & #arguments <>~ arguments

readJarManifest :: MonadIO m => Path Abs File -> m (Map Text Text)
readJarManifest fp = do
  manifest <- withArchive (toFilePath fp) $ getEntry =<< mkEntrySelector "META-INF/MANIFEST.MF"
  let go map Nothing [] = pure map
      go map (Just (k, v)) [] = pure $ map & at k ?~ v
      go map Nothing (l : ls) =
        if T.null l
          then go map Nothing ls
          else case T.splitOn sep l of
            k : (T.intercalate sep -> v) -> go map (Just (k, v)) ls
            _ -> throwString [i|invalid manifest format at line: $l|]
        where
          sep = ": "
      go map (Just (k, v)) ls' = case ls' of
        (T.stripPrefix " " -> Just l) : ls -> go map (Just (k, v <> l)) ls
        ls -> go (map & at k ?~ v) Nothing ls
  go M.empty Nothing $ lines . T.replace "\r" "" . decodeUtf8 $ manifest

getInstallerManifest ::
  (MonadIO m, MRHasAll r [DirConfig, Manager] m) => ForgeVersion -> m InstallerManifest
getInstallerManifest fv = do
  forgeFile <- reThrow $ parseRelFile [i|forge-${show fv}.json|]
  toPath <- siehs @DirConfig $ #manifestDir . to (</> forgeFile)
  decodeJSON . toLazy
    =<< extractFromInstaller fv toPath (mkEntrySelector "install_profile.json") getEntry

isServerOnlyProcessor :: Processor -> Bool
isServerOnlyProcessor p = MCServer `elem` do p ^.. #sides . #_Just . each

preprocess ::
  (MonadUnliftIO m, MRHasAll r '[MCSide, DirConfig, Manager, Logger, JavaConfig] m) =>
  ForgeVersion ->
  V.VersionManifest ->
  InstallerManifest ->
  m ()
preprocess fv vvm im = do
  side <- sieh @MCSide
  let expandVia map k = map ^. at k . non k
      expandMavenIdsAndLiterals = \case
        (bistrip "[" "]" -> Just v) ->
          Right . toText . toFilePath <$> reThrow do libraryPath =<< parseMavenId v
        (bistrip "'" "'" -> Just v) -> pure $ Right v
        v -> pure $ Left v
  (excess, dataMap) <-
    forM (im ^@.. #dataEntries . ifolded) do
      fmap (\(k, v) -> v & chosen %~ (k,)) . (_2 %%~ expandMavenIdsAndLiterals)
        . (_2 %~ view case side of MCClient -> #client; MCServer -> #server)
        . (_1 %~ \k -> "{" <> k <> "}")
      <&> partitionEithers
      <&> each %~ M.fromList
  checkPreprocessingOutputs im (expandVia dataMap) >>= \case
    Right _ -> logInfo "forge already installed"
    Left _ -> do
      logInfo "starting forge preprocessing"
      let (embedded, downloadable) =
            im ^. #libraries & partitionWith \case
              l | l & has do #downloads . #artifact . _Just . #url . only "" -> Left l
              l -> Right l
      libDir <- siehs @DirConfig #libraryDir
      forOf_ (each . #downloads . #artifact . _Just . #path) embedded \fp ->
        extractFromInstaller' fv ([reldir|maven|] </> fp) (libDir </> fp)
      downloadLibraries downloadable
      V.downloadMainJar vvm

      logInfo "running preprocessor steps"
      tmpDir <- liftIO $ getCanonicalTemporaryDirectory >>= flip createTempDirectory "forge-preprocess"
      extraDataEntries <-
        sequence . M.fromList $
          [ ("{MINECRAFT_JAR}", reThrow $ toText . toFilePath <$> V.mainJarPath vvm),
            ("{SIDE}", mcSideName)
          ]
      dataMap <-
        M.union extraDataEntries . M.union dataMap <$> do
          excess & each %%~ \(toString . T.drop 1 -> fp) -> do
            fp <- reThrow $ parseRelFile fp
            newFp <- liftIO $ parseAbsFile =<< emptyTempFile tmpDir "embedded"
            extractFromInstaller' fv fp newFp
            pure $ toText . toFilePath $ newFp
      let preprocessors =
            -- filter out the new (in forge 1.17.1) preprocessor which copies server scripts around
            im ^.. #processors . each . filtered (not . isServerOnlyProcessor)
      stepWise (withGenericProgress (length preprocessors)) \step ->
        ifor_ preprocessors \ipro pro -> step do
          jarPath <- reThrow $ libraryPath $ pro ^. #jar
          classpath <-
            reThrow $ joinClasspath . (jarPath :) <$> traverse libraryPath (pro ^. #classpath)
          args <-
            pro ^.. #args . each . to (expandVia dataMap)
              & each %%~ fmap (toString . view chosen) . expandMavenIdsAndLiterals
          mainClass <-
            rethrow . maybeToRight "no main class found" . (^? ix "Main-Class" . unpacked)
              =<< readJarManifest jarPath

          logFile <- liftIO $ emptyTempFile tmpDir [i|preprocessor-${show ipro}.log|]
          UnliftIO.IO.withFile logFile WriteMode \(useHandleOpen -> handle) -> do
            javaBin <- siehs @JavaConfig $ #javaBin . to fromSomeFile
            let p =
                  proc javaBin (["-cp", classpath, mainClass] ++ args)
                    & setStdin nullStream
                    & setStdout handle
                    & setStderr handle
            logTrace [i|raw forge preprocessor process: ${show p}|]
            runProcess p >>= #_ExitFailure \_ ->
              throwString [i|preprocessing ${show ipro} failed! log dir: $tmpDir|]
      removeDirRecur =<< reThrow (parseAbsDir tmpDir)
      checkPreprocessingOutputs im (expandVia dataMap) >>= rethrow
      logInfo "finished preprocessing"
  where
    bistrip p s = \case
      (T.stripPrefix p -> Just (T.stripSuffix s -> Just v)) -> Just v
      _ -> Nothing

checkPreprocessingOutputs ::
  MonadIO m => InstallerManifest -> (Text -> Text) -> m (Either String ())
checkPreprocessingOutputs im expander = do
  let regularOutputs =
        im ^@.. #processors . each . #outputs . _Just . ifolded
          & each %~ (_2 %~ Just) . (both %~ expander)
      -- since 1.17.1, the last preprocessing step does no longer declare its output explicitly
      -- we could also parse the --output argument
      extraOutputs = [(expander "{PATCHED}", Nothing)]
      outputs = regularOutputs <> extraOutputs
  invalidPaths <-
    catMaybes <$> forM outputs \(fp, sha1) -> do
      sha1 <- rethrow $ traverse makeSHA1 sha1
      fp <- reThrow $ parseAbsFile $ toString fp
      checkSHA1 fp sha1 <&> ($> fp) . leftToMaybe
  pure $ whenNotNull invalidPaths \(ip :| _) ->
    Left $ [i|invalid hash for processed file ${}|] $ toFilePath ip

downloadLibraries ::
  (MonadUnliftIO m, MRHasAll r '[MCSide, DirConfig, Manager] m) =>
  [V.Library] ->
  m ()
downloadLibraries =
  V.downloadArtifacts . toListOf do
    each . #downloads . #artifact . _Just . to (,Nothing)

launch ::
  ( MonadUnliftIO m,
    MRHasAll
      r
      [ MCSide,
        DirConfig,
        Manager,
        Logger,
        JavaConfig,
        GameDir,
        MCAuth
      ]
      m
  ) =>
  ForgeVersion ->
  m ()
launch fv = do
  logInfo "fetching metadata"
  fvm <- getVersionManifest fv
  libs <- reThrow $ getLibraries fvm
  vvm <- V.getVersionManifest $ fvm ^. #inheritsFrom
  isPre113 <- rethrow $ isPre113 fv
  unless isPre113 do
    im <- getInstallerManifest fv
    preprocess fv vvm im
  sieh >>= \case
    MCClient -> V.launch $ mergeWithVanilla vvm fvm libs
    MCServer -> do
      downloadLibraries libs
      when isPre113 do
        logInfo "downloading main jar"
        V.downloadMainJar vvm
      rethrow (isJigsaw fv) >>= \case
        True -> do
          argsFile <- reThrow $ parseRelFile [i|forge-args-${show fv}.txt|]
          toPath <- siehs @DirConfig $ #manifestDir . to (</> argsFile)
          libraryDir <- toFilePath <$> siehs @DirConfig #libraryDir
          let archiveArgsSelector = mkEntrySelector case Meta.os of
                Meta.Windows -> "data/win_args.txt"
                _ -> "data/unix_args.txt"
              patchLibraryDir =
                ( prefixed "-p "
                    `adjoin` prefixed "-DlegacyClassPath="
                    `adjoin` prefixed "-DlibraryDirectory="
                )
                  %~ \case
                    "libraries" -> libraryDir
                    classpath ->
                      intercalate classpathSeparator
                        . fmap (libraryDir <>)
                        . mapMaybe (stripPrefix "libraries/")
                        . splitOn classpathSeparator
                        $ classpath
              extractArgs = toListOf $ lined . to patchLibraryDir . worded
          args <- extractArgs . decodeUtf8 <$> extractFromInstaller fv toPath archiveArgsSelector getEntry
          runMCJava args
        False -> do
          launcherJar <-
            libs ^? each . #name . filtered (has $ #artifactId . only "forge")
              & rethrow . maybeToRight "launcher library not found" >>= reThrow . libraryPath
          jarManifest <- readJarManifest launcherJar
          let expectEntry (k :: String) = rethrow . maybeToRight [i|$k not found|]
          mainClass <- expectEntry "main class" $ jarManifest ^? ix "Main-Class" . unpacked
          otherLibs <- do
            raw <- expectEntry "classpath" $ jarManifest ^? ix "Class-Path" . to (T.splitOn " ")
            libDir <- siehs @DirConfig #libraryDir
            raw & each %%~ \case
              (T.stripPrefix "libraries/" -> Just p) -> do
                p <- reThrow $ parseRelFile $ toString p
                pure $ libDir </> p
              p | "minecraft_server." `T.isPrefixOf` p -> reThrow $ V.mainJarPath vvm
              p -> throwString [i|invalid classpath entry: $p|]
          runMCJava $ ["-cp", joinClasspath $ launcherJar : otherLibs] ++ [mainClass]
