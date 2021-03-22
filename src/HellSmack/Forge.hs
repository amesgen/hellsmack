module HellSmack.Forge
  ( ForgeVersion (..),
    isPre113,
    ForgeVersionQuery (..),
    allVersionsManifestPath,
    findForgeVersion,
    getVersionManifest,
    launch,
  )
where

import Codec.Archive.Zip
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types (parseEither)
import Data.Conduit.Process.Typed
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Lens
import Data.Time
import HellSmack.Logging
import HellSmack.Util
import HellSmack.Vanilla qualified as V
import HellSmack.Yggdrasil
import Path.IO
import System.IO.Temp
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception
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
-- Right False
-- >>> isPre113 (ForgeVersion "1.15-29.0.1")
-- Right False
-- >>> isPre113 (ForgeVersion "1.12.1-14.22.1.2480")
-- Right True
-- >>> isPre113 (ForgeVersion "1.4.5-6.4.2.447")
-- Right True
-- >>> isPre113 (ForgeVersion "1.7.10-10.13.2.1352-1.7.10")
-- Right True

isPre113 :: ForgeVersion -> Either String Bool
isPre113 (ForgeVersion v) = maybeToRight "invalid forge version format" case v
  & T.takeWhile (/= '-')
  & T.splitOn "."
  & take 2
  & each %%~ readMaybe . toString of
  Just [maj, min] -> pure $ (maj, min) < (1 :: Int, 13)
  _ -> Nothing

isPre113M :: MonadIO m => ForgeVersion -> m Bool
isPre113M = rethrow . isPre113

data ForgeVersionQuery = LatestFV | RecommendedFV | ConcreteFV Text
  deriving stock (Show, Generic)

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

instance FromJSON VersionManifest where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "versionType" -> "type"
            l -> l
        }

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
  { path :: MavenId,
    dataEntries :: Map Text DataEntry,
    processors :: [Processor],
    libraries :: [V.Library]
  }
  deriving stock (Show, Generic)

instance FromJSON InstallerManifest where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "dataEntries" -> "data"
            l -> l
        }

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
    outputs :: Maybe (Map Text Text)
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

allVersionsManifestPath :: MRHas r DirConfig m => m (Path Abs File)
allVersionsManifestPath =
  siehs @DirConfig $ #manifestDir . to (</> [relfile|forgeVersionManifest.json|])

findForgeVersion ::
  (MonadIO m, MRHasAll r [DirConfig, Manager] m) =>
  V.MCVersion ->
  ForgeVersionQuery ->
  m ForgeVersion
findForgeVersion (coerce -> mcVersion) fvq = do
  manifestPath <- allVersionsManifestPath
  let promoOrVersion = case fvq of
        ConcreteFV version -> Right version
        RecommendedFV -> Left ("recommended" :: Text)
        LatestFV -> Left "latest"
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
  isPre113 <- isPre113M fv
  let findVersionFile = do
        es <- mkEntrySelector "version.json"
        doesEntryExist es >>= \case
          True -> pure es
          False -> mkEntrySelector "install_profile.json"
  forgeFile <- reThrow $ parseRelFile [i|forge-${show fv}.json|]
  toPath <- siehs @DirConfig $ #manifestDir . to (</> forgeFile)
  json :: Value <-
    rethrow . eitherDecodeStrict' =<< extractFromInstaller fv toPath findVersionFile getEntry
  vm <- fromJSON case (json ^? key "install" . key "minecraft", json ^? key "versionInfo") of
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
    fromJSON = rethrow . parseEither parseJSON
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
  rethrow . eitherDecodeStrict'
    =<< extractFromInstaller fv toPath (mkEntrySelector "install_profile.json") getEntry

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
      mainJarPath <- reThrow $ toText . toFilePath <$> V.mainJarPath vvm
      dataMap <-
        (dataMap <>) . (at "{MINECRAFT_JAR}" ?~ mainJarPath) <$> do
          excess & each %%~ \(toString . T.drop 1 -> fp) -> do
            fp <- reThrow $ parseRelFile fp
            newFp <- liftIO $ parseAbsFile =<< emptyTempFile tmpDir "embedded"
            extractFromInstaller' fv fp newFp
            pure $ toText . toFilePath $ newFp
      stepWise (withGenericProgress (length $ im ^. #processors)) \step ->
        iforOf_ (#processors . ifolded) im \ipro pro -> step do
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
            runProcess p >>= traverseOf #_ExitFailure \_ -> do
              logInfo [i|preprocessing log dir: $tmpDir|]
              throwString [i|preprocessing ${show ipro} failed!|]
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
  let outputs = im ^@.. #processors . each . #outputs . _Just . ifolded & each . each %~ expander
  invalidPaths <-
    catMaybes <$> forM outputs \(fp, sha1) -> do
      sha1 <- rethrow $ makeSHA1 sha1
      fp <- reThrow $ parseAbsFile $ toString fp
      checkSha1 fp (Just sha1) <&> ($> fp) . leftToMaybe
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
  isPre113 <- isPre113M fv
  unless isPre113 do
    im <- getInstallerManifest fv
    preprocess fv vvm im
  sieh >>= \case
    MCClient -> do
      let vm = mergeWithVanilla vvm fvm libs
      V.launch vm
    MCServer -> do
      downloadLibraries libs
      when isPre113 do
        logInfo "downloading main jar"
        V.downloadMainJar vvm
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
