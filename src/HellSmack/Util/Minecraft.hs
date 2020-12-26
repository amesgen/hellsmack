module HellSmack.Util.Minecraft
  ( DirConfig (..),
    newDirConfig,
    GameDir (getGameDir),
    newGameDir,
    MCSide (..),
    mcSideName,
    MavenId (..),
    parseMavenId,
    mavenIdUrl,
    mavenIdPath,
    libraryPath,
    joinClasspath,
    JavaConfig (..),
    runMCJava,
  )
where

import Data.Aeson
import Data.Conduit.Process.Typed
import Data.Text qualified as T
import HellSmack.Logging
import HellSmack.Util.Has
import Path.IO
import System.Exit
import System.FilePath (searchPathSeparator)
import UnliftIO.Exception
import UnliftIO.IO.File

data DirConfig = DirConfig
  { assetDir :: Path Abs Dir,
    libraryDir :: Path Abs Dir,
    versionDir :: Path Abs Dir,
    manifestDir :: Path Abs Dir,
    nativeDir :: Path Abs Dir
  }
  deriving stock (Show, Generic)

ensureAbsDir :: MonadIO m => Path Abs Dir -> m (Path Abs Dir)
ensureAbsDir dir = do
  ensureDir dir
  makeAbsolute dir

newDirConfig ::
  MonadIO m =>
  -- | base dir
  Path Abs Dir ->
  m DirConfig
newDirConfig fp = do
  (assetDir, libraryDir, versionDir, manifestDir, nativeDir) <-
    ( [reldir|assets|],
      [reldir|libraries|],
      [reldir|versions|],
      [reldir|manifests|],
      [reldir|natives|]
      )
      & each %~ (fp </>)
      & each %%~ ensureAbsDir

  -- see https://bford.info/cachedir/
  let cacheDirTag = fp </> [relfile|CACHEDIR.TAG|]
  unlessM (doesFileExist cacheDirTag) $
    writeBinaryFileDurableAtomic
      (toFilePath cacheDirTag)
      "Signature: 8a477f597d28d172789f06886806bc55"

  pure DirConfig {..}

newtype GameDir = GameDir {getGameDir :: Path Abs Dir}
  deriving stock (Show, Generic)

newGameDir :: MonadIO m => Path Abs Dir -> m GameDir
newGameDir = fmap coerce . ensureAbsDir

data MavenId = MavenId
  { groupId :: Text,
    artifactId :: Text,
    version :: Text,
    classifier :: Maybe Text,
    extension :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON MavenId where
  parseJSON = parseJSON >=> parseMavenId

parseMavenId :: MonadFail m => Text -> m MavenId
parseMavenId mi = case T.breakOn extSep mi of
  ( parseRest -> Just (groupId, artifactId, version, classifier),
    T.stripPrefix extSep -> extension
    ) -> pure MavenId {..}
  _ -> fail [i|invalid maven identifier: #{mi}|]
  where
    extSep = "@"
    parseRest =
      T.splitOn ":" >>> \case
        [g, n, v] -> Just (g, n, v, Nothing)
        [g, n, v, c] -> Just (g, n, v, Just c)
        _ -> Nothing

mavenIdUrl :: MavenId -> Text
mavenIdUrl MavenId {..} =
  let g = T.splitOn "." groupId
      c = maybe "" ("-" <>) classifier
      e = extension ?: "jar"
   in T.intercalate "/" $ g <> [artifactId, version, [i|#{artifactId}-#{version}#{c}.#{e}|]]

mavenIdPath :: MonadThrow m => MavenId -> m (Path Rel File)
mavenIdPath = parseRelFile . toString . mavenIdUrl

libraryPath :: (MRHas r DirConfig m, MonadThrow m) => MavenId -> m (Path Abs File)
libraryPath mi = (</>) <$> siehs @DirConfig #libraryDir <*> mavenIdPath mi

data MCSide = MCClient | MCServer
  deriving stock (Show, Ord, Eq, Generic, Enum, Bounded)

mcSideName :: MRHas r MCSide m => m Text
mcSideName =
  sieh <&> \case
    MCClient -> "client"
    MCServer -> "server"

joinClasspath :: [Path Abs File] -> String
joinClasspath = fold . intersperse [searchPathSeparator] . fmap toFilePath

data JavaConfig = JavaConfig
  { javaBin :: SomeBase File,
    extraJvmArgs :: [String]
  }
  deriving stock (Show, Generic)

runMCJava :: (MonadIO m, MRHasAll r [JavaConfig, GameDir, Logger] m) => [String] -> m ()
runMCJava jvmArgs = do
  JavaConfig {..} <- sieh
  gdir <- sieh <&> toFilePath . getGameDir
  let p = proc (fromSomeFile javaBin) (extraJvmArgs ++ jvmArgs) & setWorkingDir gdir
  logInfo "launching minecraft"
  logTrace [i|raw command: #{p}|]
  runProcess p >>= \case
    ExitSuccess -> logInfo "minecraft finished successfully"
    _ -> throwString "minecraft crashed :("
