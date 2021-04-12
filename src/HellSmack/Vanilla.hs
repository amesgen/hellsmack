module HellSmack.Vanilla
  ( -- * Types
    MCVersion (..),
    AllVersionsManifest (..),
    LatestVersion (..),
    Version (..),
    VersionType (..),
    VersionManifest (..),
    Arguments (..),
    Argument (..),
    Rule (..),
    FromJSON (..),
    Action (..),
    Property (..),
    AssetIndex (..),
    Download (..),
    Library (..),
    LibraryDownload (..),
    OSName,
    Extract (..),
    Artifact (..),
    Assets (..),
    Asset (..),

    -- * Utilities

    -- ** Version manifests
    allVersionsManifestPath,
    getAllVersionsManifest,
    getVersionManifest,

    -- ** Assets
    getAssetIndex,
    AssetsType (..),
    assetsType,
    downloadAssets,

    -- ** Rules
    doesPropertyApply,
    processRules,

    -- ** Artifacts
    artifactPath,
    processArtifacts,
    downloadArtifacts,

    -- ** Main JARs
    mainJarPath,
    downloadMainJar,

    -- ** Arguments
    classpathArg,
    ProcessedArguments (..),
    processArguments,

    -- * Launch
    launch,
  )
where

import Codec.Archive.Zip
import Conduit
import Data.Aeson
import Data.Aeson.Lens
import Data.Semigroup.Generic
import Data.Text.Lens
import Data.Time
import HellSmack.Logging
import HellSmack.Util
import HellSmack.Util.Meta qualified as Meta
import HellSmack.Yggdrasil
import Path.IO
import Relude.Debug qualified as RU
import Text.Regex.Pcre2
import UnliftIO.Exception

data AllVersionsManifest = AllVersionsManifest
  { latest :: LatestVersion,
    versions :: [Version]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data LatestVersion = LatestVersion
  { release :: MCVersion,
    snapshot :: MCVersion
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data Version = Version
  { id :: MCVersion,
    versionType :: VersionType,
    url :: Text,
    time :: UTCTime,
    releaseTime :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Version where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "versionType" -> "type"
            l -> l
        }

data VersionType = Release | Snapshot | OldAlpha | OldBeta
  deriving stock (Show, Eq, Generic, Enum, Bounded)

instance FromJSON VersionType where
  parseJSON =
    parseJSON @Text >=> \case
      "release" -> pure Release
      "snapshot" -> pure Snapshot
      "old_alpha" -> pure OldAlpha
      "old_beta" -> pure OldBeta
      vt -> fail [i|invalid version type: $vt|]

data VersionManifest = VersionManifest
  { arguments :: Maybe Arguments,
    minecraftArguments :: Maybe Text,
    assetIndex :: AssetIndex,
    assets :: Text,
    downloads :: Map Text Download,
    id :: MCVersion,
    libraries :: [Library],
    -- logging :: Maybe Logging,
    mainClass :: Text,
    time :: UTCTime,
    releaseTime :: UTCTime,
    versionType :: VersionType
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON VersionManifest where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "versionType" -> "type"
            l -> l
        }

data Arguments = Arguments
  { game :: [Argument],
    jvm :: [Argument]
  }
  deriving stock (Show, Eq, Generic)
  deriving (Semigroup, Monoid) via (GenericSemigroupMonoid Arguments)

instance FromJSON Arguments where
  parseJSON = withObject "Arguments" \v -> do
    game <- v .:? "game" .!= []
    jvm <- v .:? "jvm" .!= []
    pure Arguments {..}

data Argument = Argument
  { value :: [Text],
    rules :: [Rule]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Argument where
  parseJSON v = s <|> c
    where
      s = parseJSON v <&> \t -> Argument {value = [t], rules = []}
      c =
        v & withObject "Argument" \v -> do
          value <- (pure <$> v .: "value") <|> (v .: "value")
          rules <- v .: "rules"
          pure Argument {..}

data Rule = Rule
  { action :: Action,
    properties :: [Property]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Rule where
  parseJSON = withObject "Rule" \v -> do
    action <- v .: actionKey
    let properties =
          Object v
            ^.. do members . indices (/= actionKey) <.> members
              . withIndex
              . to do uncurry Property
    pure Rule {..}
    where
      actionKey = "action"

data Action = Allow | Disallow
  deriving stock (Show, Eq, Generic, Enum, Bounded)

instance FromJSON Action where
  parseJSON =
    parseJSON @Text >=> \case
      "allow" -> pure Allow
      "disallow" -> pure Disallow
      a -> fail [i|invalid action: $a|]

data Property = Property
  { key :: (Text, Text),
    value :: Value
  }
  deriving stock (Show, Eq, Generic)

data AssetIndex = AssetIndex
  { id :: Text,
    sha1 :: SHA1,
    size :: Word64,
    totalSize :: Word64,
    url :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data Download = Download
  { sha1 :: SHA1,
    size :: Word64,
    url :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data Library = Library
  { downloads :: LibraryDownload,
    name :: MavenId,
    natives :: Maybe (Map OSName Text),
    extract :: Maybe Extract,
    rules :: Maybe [Rule]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data LibraryDownload = LibraryDownload
  { artifact :: Maybe Artifact,
    classifiers :: Maybe (Map Text Artifact)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

newtype OSName = OSName Text
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, FromJSONKey)

newtype Extract = Extract
  { exclude :: [FilePath]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data Artifact = Artifact
  { path :: Path Rel File,
    sha1 :: Maybe SHA1,
    size :: Maybe Word64,
    url :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data Assets = Assets
  { objects :: Map (Path Rel File) Asset,
    virtual :: Maybe Bool,
    map_to_resources :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data Asset = Asset
  { hash :: SHA1,
    size :: Word64
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

--

allVersionsManifestUrl :: Text
allVersionsManifestUrl = "https://launchermeta.mojang.com/mc/game/version_manifest.json"

allVersionsManifestPath :: MRHas r DirConfig m => m (Path Abs File)
allVersionsManifestPath =
  siehs @DirConfig $ #manifestDir . to (</> [relfile|allVersionsManifest.json|])

getAllVersionsManifest ::
  (MonadIO m, MRHasAll r [DirConfig, Manager] m) => m AllVersionsManifest
getAllVersionsManifest = do
  path <- allVersionsManifestPath
  downloadCachedJson allVersionsManifestUrl path Nothing

getVersionManifest ::
  (MonadIO m, MRHasAll r [DirConfig, Manager] m) =>
  MCVersion ->
  m VersionManifest
getVersionManifest version = do
  path <- allVersionsManifestPath
  Version {url, id} <- downloadMaybeJson allVersionsManifestUrl path Nothing \avm ->
    (avm :: AllVersionsManifest)
      & findOf (#versions . each) (has $ #id . only version)
      & maybeToRight [i|minecraft version '${show version}' not found|]
  versionPath <- reThrow $ (</>) <$> siehs @DirConfig #manifestDir <*> parseRelFile [i|${show id}.json|]
  let hashFromUrl = url & T.stripPrefix urlPrefix <&> T.takeWhile (/= '/') >>= makeSHA1
  downloadCachedJson url versionPath hashFromUrl
  where
    urlPrefix = "https://launchermeta.mojang.com/v1/packages/"

getAssetIndex :: (MonadIO m, MRHasAll r [DirConfig, Manager] m) => VersionManifest -> m Assets
getAssetIndex vm = do
  let AssetIndex {id, url, sha1} = vm ^. #assetIndex
  idFile <- reThrow . parseRelFile $ [i|$id.json|]
  assetIndexPath <- siehs @DirConfig $ #assetDir . to (</> [reldir|indexes|] </> idFile)
  downloadCachedJson url assetIndexPath (Just sha1)

assetUrl :: Text
assetUrl = "https://resources.download.minecraft.net"

data AssetsType = ModernAssetsType | LegacyAssetsType | Pre16AssetsType
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)

assetsType :: MonadFail m => Assets -> m AssetsType
assetsType assets = case (assets ^. #virtual, assets ^. #map_to_resources) of
  (Nothing, Nothing) -> pure ModernAssetsType
  (Just True, Nothing) -> pure LegacyAssetsType
  (Nothing, Just True) -> pure Pre16AssetsType -- actually support this?
  _ -> fail [i|cannot detect assets type|]

assetObjectsDir :: (MonadFail m, MRHas r DirConfig m) => Assets -> m (Path Abs Dir)
assetObjectsDir assets =
  assetsType assets >>= \case
    ModernAssetsType -> assetsSubDir [reldir|objects|]
    LegacyAssetsType -> assetsSubDir [reldir|virtual/legacy|]
    Pre16AssetsType -> fail "invalid assets type"
  where
    assetsSubDir sub = siehs @DirConfig #assetDir <&> (</> sub)

downloadAssets ::
  (MonadUnliftIO m, MRHasAll r [DirConfig, Manager, MCSide, Logger] m) =>
  Assets ->
  m ()
downloadAssets assets@Assets {objects} = do
  baseDir <- reThrow $ assetObjectsDir assets
  let forAllAssets getPath = stepWise (withGenericProgress (length objects)) \step ->
        forConcurrentlyNetwork_ (M.toList objects) \e@(_, Asset {hash}) -> step do
          path <- getPath e
          let url = [i|$assetUrl/${}|] $ h2hash hash
          downloadHash url path (Just hash) HideProgress
      h2hash (unSHA1 -> hash) = [iS|${}/$hash|] $ T.take 2 hash
  rethrow (assetsType assets) >>= \case
    ModernAssetsType -> forAllAssets \(_, Asset {hash}) -> do
      file <- reThrow $ parseRelFile $ h2hash hash
      pure $ baseDir </> file
    LegacyAssetsType -> forAllAssets \(file, _ö) -> do
      let path = baseDir </> file
      ensureDir $ parent path
      pure path
    Pre16AssetsType -> throwString "invalid assets type"

doesPropertyApply :: Property -> Bool
doesPropertyApply Property {key, value} = case knownProperties ^? ix key of
  Just applies -> applies value
  Nothing -> let (k, v) = key in RU.error [i|unknown property: $k.$v|]
  where
    knownProperties =
      M.fromList
        [ (("features", "is_demo_user"), has $ _Bool . only False),
          (("features", "has_custom_resolution"), has $ _Bool . only False),
          (("os", "name"), has $ _String . only currentOSName),
          (("os", "arch"), has $ _String . only currentArchName),
          (("os", "version"), const False) -- TODO windows 10?
        ]

currentOSName :: Text
currentOSName = case Meta.os of
  Meta.Linux -> "linux"
  Meta.OSX -> "osx"
  Meta.Windows -> "windows"

currentArchName :: Text
currentArchName = case Meta.arch of
  Meta.X86 -> "x86"
  Meta.X86_64 -> "amd64" -- NOTE confirm

processRules :: [Rule] -> Action
processRules rules
  | null rules = Allow
  | otherwise =
    rules
      & takeWhile (allOf (#properties . each) doesPropertyApply)
      & preview _last
      & maybe Disallow (view #action)

type Artifacts = [(Artifact, Maybe (Extract, Path Abs Dir))]

processArtifacts :: (MonadThrow m, MRHas r DirConfig m) => VersionManifest -> m Artifacts
processArtifacts vm = do
  nativeDir <- versionNativeDir vm
  let validArtifact = (Allow ==) . processRules . toListOf do #rules . _Just . each
      fromLib Library {..} = case downloads ^. #classifiers of
        Just cls ->
          natives
            ^.. each
              . ifolded
              . index do OSName currentOSName
              . to do \n -> cls ^? ix n <&> (,extract <&> (,nativeDir))
              . each
        Nothing -> downloads ^.. #artifact . each . to (,Nothing)
  pure $ vm ^.. #libraries . each . filtered validArtifact . to fromLib . each

versionNativeDir :: (MonadThrow m, MRHas r DirConfig m) => VersionManifest -> m (Path Abs Dir)
versionNativeDir vm = do
  nativeDir <- siehs @DirConfig #nativeDir
  vmid <- parseRelDir $ vm ^. #id . to unMCVersion . unpacked
  pure $ nativeDir </> vmid

artifactPath :: (MRHas r DirConfig m) => Artifact -> m (Path Abs File)
artifactPath Artifact {path} = siehs @DirConfig $ #libraryDir . to (</> path)

downloadArtifacts ::
  (MonadUnliftIO m, MRHasAll r [DirConfig, Manager, MCSide] m) =>
  Artifacts ->
  m ()
downloadArtifacts afs = stepWise (withGenericProgress (length afs)) \step ->
  forConcurrentlyNetwork_ afs \(a@Artifact {..}, ex) -> step do
    fullPath <- artifactPath a
    downloadHash url fullPath sha1 HideProgress
    for_ ex \(Extract {..}, targetDir) -> do
      withArchive (toFilePath fullPath) do
        entries <-
          getEntries <&> toListOf do
            ifolded
              . indices do \e -> exclude & all \ex -> not $ ex `isPrefixOf` unEntrySelector e
              . asIndex
        for_ entries \e -> do
          extractPath <- (targetDir </>) <$> parseRelFile (unEntrySelector e)
          ensureDir $ parent extractPath
          unlessM (doesFileExist extractPath) $
            saveEntry e (toFilePath extractPath)

getMainDownload :: MRHas r MCSide m => VersionManifest -> m (Maybe Download)
getMainDownload vm = mcSideName <&> \side -> vm ^. #downloads . at side

mainJarPath ::
  (MonadThrow m, MRHasAll r [DirConfig, MCSide] m) =>
  VersionManifest ->
  m (Path Abs File)
mainJarPath vm = do
  side <- mcSideName
  versionDir <- siehs @DirConfig #versionDir
  vmid <- parseRelDir $ vm ^. #id . to unMCVersion . unpacked
  sideJar <- parseRelFile [i|$side.jar|]
  pure $ versionDir </> vmid </> sideJar

downloadMainJar ::
  (MonadUnliftIO m, MRHasAll r [DirConfig, MCSide, Manager] m) => VersionManifest -> m ()
downloadMainJar vm = whenJustM (getMainDownload vm) \Download {..} -> do
  path <- reThrow $ mainJarPath vm
  downloadHash url path (Just sha1) ShowProgress

classpathArg ::
  (MonadThrow m, MRHasAll r [DirConfig, MCSide] m) =>
  VersionManifest ->
  Artifacts ->
  m String
classpathArg vm artifacts = do
  artifactPaths <- artifacts <&> fst & each %%~ artifactPath
  maybeMainJarPath <-
    getMainDownload vm >>= \case
      Just _ -> pure <$> mainJarPath vm
      _ -> pure []
  pure . joinClasspath $ artifactPaths <> maybeMainJarPath

data ProcessedArguments = ProcessedArguments
  { game :: [String],
    jvm :: [String]
  }
  deriving stock (Show, Generic)

processArguments ::
  (MonadIO m, MRHasAll r [DirConfig, MCSide, GameDir, MCAuth] m) =>
  VersionManifest ->
  Assets ->
  String ->
  m ProcessedArguments
processArguments vm assets classpath = do
  let VersionManifest {minecraftArguments = oldArgs, arguments = newArgs} = vm
  uncurry ProcessedArguments . fold . catMaybes
    <$> sequence [forM oldArgs processOldArgs, forM newArgs processNewArgs]
  where
    processNewArgs Arguments {game, jvm} =
      (game, jvm) & each %%~ \args -> traverse replaceInputs do
        Argument {value, rules} <- args
        guard $ processRules rules == Allow
        toString <$> value
    processOldArgs args = do
      game <- T.splitOn " " args <&> toString & traverse replaceInputs
      natDir <- reThrow $ toFilePath <$> versionNativeDir vm
      pure (game, ["-cp", classpath, [i|-Djava.library.path=$natDir|]])
    replaceInputs =
      packed . [_regex|\$\{(?<prop>[\S]+)\}|] %%~ \cap ->
        cap & _capture @0 %%~ const case cap ^. _capture @"prop" of
          "classpath" -> pure $ toText classpath
          "natives_directory" -> fromPath $ versionNativeDir vm
          "game_assets" -> fromPath $ assetObjectsDir assets
          "assets_root" -> fromPath $ siehs @DirConfig #assetDir
          "assets_index_name" -> pure $ vm ^. #assetIndex . #id
          "game_directory" -> fromPath $ siehs @GameDir $ to unGameDir
          "auth_player_name" -> siehs @MCAuth #username
          "auth_uuid" -> siehs @MCAuth #uuid
          ((`elem` ["auth_access_token", "auth_session"]) -> True) ->
            siehs @MCAuth $ #accessToken . to unAccessToken
          ((`elem` ["version_name", "launcher_name"]) -> True) -> pure Meta.name
          "launcher_version" -> pure Meta.version
          "version_type" -> pure case vm ^. #versionType of
            Release -> "release"
            Snapshot -> "snapshot"
            _ -> "unknown"
          "user_type" -> pure "mojang" -- NOTE legacy?
          "user_properties" -> pure "{}"
          input -> throwString [i|invalid input '${input}' in argument|]
    fromPath = reThrow . fmap (toText . toFilePath)

launch ::
  ( MonadUnliftIO m,
    MRHasAll
      r
      [ DirConfig,
        MCSide,
        Logger,
        Manager,
        GameDir,
        MCAuth,
        JavaConfig
      ]
      m
  ) =>
  VersionManifest ->
  m ()
launch vm =
  sieh >>= \case
    MCClient -> do
      logInfo "downloading assets"
      assets <- getAssetIndex vm
      downloadAssets assets
      artifacts <- reThrow $ processArtifacts vm
      logInfo "downloading artifacts"
      downloadArtifacts artifacts
      logInfo "downloading main jar"
      downloadMainJar vm
      logInfo "preparing launch"
      classpath <- reThrow $ classpathArg vm artifacts
      ProcessedArguments {..} <- processArguments vm assets classpath
      let mainClass = vm ^. #mainClass . unpacked
      runMCJava $ jvm <> [mainClass] <> game
    MCServer -> do
      logInfo "downloading main jar"
      downloadMainJar vm
      mjp <- reThrow $ mainJarPath vm
      runMCJava ["-jar", toFilePath mjp]
