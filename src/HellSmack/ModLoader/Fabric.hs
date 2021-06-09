module HellSmack.ModLoader.Fabric
  ( FabricVersion (..),
    findVersion,
    allVersionsManifestPath,
    launch,
  )
where

import Data.Time
import HellSmack.Logging
import HellSmack.ModLoader
import HellSmack.Util
import HellSmack.Vanilla qualified as V
import HellSmack.Yggdrasil
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Status qualified as HTTP
import UnliftIO.Exception

newtype FabricVersion = FabricVersion {unFabricVersion :: Text}
  deriving stock (Generic)
  deriving newtype (Ord, Eq, FromJSON)
  deriving (Show) via (ShowWithoutQuotes Text)

data Loader = Loader
  { version :: FabricVersion,
    stable :: Bool
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data VersionManifest = VersionManifest
  { id :: Text,
    inheritsFrom :: MCVersion,
    time :: UTCTime,
    versionType :: V.VersionType,
    mainClass :: Text,
    arguments :: V.Arguments,
    libraries :: [Library]
  }
  deriving stock (Show, Generic)
  deriving (FromJSON) via CustomJSONLabel '[Rename "versionType" "type"] VersionManifest

data Library = Library
  { name :: MavenId,
    url :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

fabricMeta :: Text
fabricMeta = "https://meta.fabricmc.net"

allVersionsManifestPath :: MRHas r DirConfig m => m (Path Abs File)
allVersionsManifestPath =
  siehs @DirConfig $ #manifestDir . to (</> [relfile|fabricLoaders.json|])

findVersion ::
  (MonadUnliftIO m, MRHasAll r [DirConfig, Manager, Logger] m) =>
  VersionQuery ->
  m FabricVersion
findVersion fvq = do
  loadersPath <- allVersionsManifestPath
  when (isn't #_ConcreteVersion fvq) $ removeFileForce loadersPath
  Loader {..} <- downloadMaybeJson loadersUrl loadersPath Nothing \(ls :: [Loader]) -> do
    let firstWith p = ls ^? each . filtered p
    case fvq of
      LatestVersion ->
        maybeToRight "no Fabric version found" $ firstWith (const True)
      RecommendedVersion ->
        maybeToRight "no stable Fabric version found" $ firstWith (^. #stable)
      ConcreteVersion v ->
        maybeToRight [i|$v is not a known Fabric version|] $ firstWith (has $ #version . #_FabricVersion . only v)
  unless stable $ logWarn [i|the Fabric version ${show version} is unstable|]
  pure version
  where
    loadersUrl = [i|$fabricMeta/v2/versions/loader|]

getVersionManifest ::
  (MonadUnliftIO m, MRHasAll r [DirConfig, Manager, Logger, MCSide] m) =>
  V.MCVersion ->
  FabricVersion ->
  m VersionManifest
getVersionManifest mcVersion fabricVersion = do
  sideName <- mcSideName
  manifestFile <- reThrow $ parseRelFile [i|fabric-${show mcVersion}-$sideName-${show fabricVersion}.json|]
  manifestPath <- siehs @DirConfig $ #manifestDir . to (</> manifestFile)
  sideSlug :: Text <- sieh <&> \case MCClient -> "profile"; MCServer -> "server"
  let manifestUrl = [i|$fabricMeta/v2/versions/loader/${show mcVersion}/${show fabricVersion}/$sideSlug/json|]
  downloadCachedJson manifestUrl manifestPath Nothing `catch` \case
    HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException res body)
      | HTTP.responseStatus res == HTTP.badRequest400 && "no mappings version found for" `B.isPrefixOf` body ->
        throwString "chosen Fabric version does not support this Minecraft version"
    e -> throwIO e

libraryToArtifact :: MonadThrow m => Library -> m V.Artifact
libraryToArtifact Library {..} = do
  let (size, sha1) = (Nothing, Nothing)
  path <- mavenIdPath name
  pure V.Artifact {url = url <> mavenIdUrl name, ..}

mergeWithVanilla :: MonadThrow m => V.VersionManifest -> VersionManifest -> m V.VersionManifest
mergeWithVanilla vm VersionManifest {..} = do
  libraries <- forM libraries migrateLibrary
  pure $
    vm
      & #id .~ MCVersion id
      & #time .~ time
      & #versionType .~ versionType
      & #mainClass .~ mainClass
      & #arguments . _Just <>~ arguments
      & #libraries %~ (libraries <>)
  where
    migrateLibrary l@Library {..} = do
      (Just -> artifact) <- libraryToArtifact l
      let (natives, extract, rules, classifiers) = (Nothing, Nothing, Nothing, Nothing)
          downloads = V.LibraryDownload {..}
      pure V.Library {..}

launch ::
  ( MonadUnliftIO m,
    MRHasAll r [MCSide, DirConfig, Manager, Logger, JavaConfig, GameDir, MCAuth] m
  ) =>
  MCVersion ->
  FabricVersion ->
  m ()
launch mcVersion fabricVersion = do
  logInfo "fetching metadata"
  fvm <- getVersionManifest mcVersion fabricVersion
  vvm <- V.getVersionManifest $ fvm ^. #inheritsFrom
  sieh >>= \case
    MCClient -> do
      vm <- reThrow $ mergeWithVanilla vvm fvm
      V.launch vm
    MCServer -> do
      logInfo "downloading main jar"
      V.downloadMainJar vvm
      (fmap (,Nothing) -> artifacts) <- reThrow $ forM (fvm ^. #libraries) libraryToArtifact
      logInfo "downloading artifacts"
      V.downloadArtifacts artifacts
      cp <- reThrow $ V.classpathArg vvm artifacts
      runMCJava ["-cp", cp, toString $ fvm ^. #mainClass]
