{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

import Colourista.Pure qualified as C
import Data.Text.Lens
import HellSmack.Curse qualified as Curse
import HellSmack.Curse.API qualified as Curse
import HellSmack.Http
import HellSmack.Logging
import HellSmack.ModLoader qualified as ModLoader
import HellSmack.ModLoader.Fabric qualified as Fabric
import HellSmack.ModLoader.Forge qualified as Forge
import HellSmack.Util
import HellSmack.Util.Meta qualified as Meta
import HellSmack.Vanilla qualified as Vanilla
import HellSmack.Yggdrasil
import Language.Haskell.TH.Env
import Main.Utf8 (withUtf8)
import Options.Applicative qualified as OA
import System.Exit (ExitCode (..))
import UnliftIO.Exception

main :: IO ()
main =
  withUtf8 . displayErrors $ do
    -- TODO use some open product thingy (cf. jrec branch)
    getCLI >>= \CLI {..} -> do
      dataDir <- maybe defaultDataDir makeSomeAbsolute dataDir
      ensureDir dataDir
      let authPath = dataDir </> [relfile|auth.json|]
          logger = simpleLogger verbosity
      httpManager <- newTLSManager
      case cmds of
        Launch LaunchOptions {..} -> do
          dirConfig <- newDirConfig dataDir
          gameDir <- newGameDir =<< makeSomeAbsolute gameDir
          mcAuth <-
            if authenticate && mcSide == MCClient
              then usingReaderT (httpManager, logger) $ loadMCAuth authPath
              else pure bogusMCAuth
          let javaConfig = JavaConfig {..}
          usingReaderT MCConfig {..} do
            sideName <- mcSideName
            case (forgeVersion, fabricVersion) of
              (Nothing, Nothing) -> do
                vm <- Vanilla.getVersionManifest mcVersion
                logInfo [i|launching Minecraft ${inGreen mcVersion} ($sideName)|]
                Vanilla.launch vm
              (Just forgeVersion, Nothing) -> do
                fv <- Forge.findVersion mcVersion forgeVersion
                logInfo [i|launching Minecraft Forge ${inGreen fv} ($sideName)|]
                Forge.launch fv
              (Nothing, Just fabricVersion) -> do
                fv <- Fabric.findVersion fabricVersion
                logInfo [i|launching Minecraft ${inGreen mcVersion}, Fabric ${inGreen fv} ($sideName)|]
                Fabric.launch mcVersion fv
              _ -> throwString "more than one mod loader specified!"
        Auth o -> usingReaderT (httpManager, logger) case o of
          Login LoginOptions {..} -> saveMCAuth authPath email password
          Logout -> invalidateMCAuth authPath
          Verify -> void $ loadMCAuth authPath
        Curse o -> usingReaderT (httpManager, logger) case o of
          Modpacks o -> case o of
            ModpackInstall o ->
              Curse.downloadFullModpack o
            ModpackSearchInstall o ->
              Curse.searchInstallModpack o
          Mods o -> case o of
            ModUpdates o ->
              Curse.updateMods o
            ModInstall o ->
              Curse.installMods o
            ModSearchInstall o ->
              Curse.searchInstallMod o
            ModDeduplicate o ->
              Curse.deduplicateMods o
  where
    inGreen = C.formatWith [C.green] . display
    displayErrors =
      catches
        ?? [ Handler \(StringException msg cs) -> logAndExit $ toText $ msg <> "\n" <> prettyCallStack cs,
             Handler \(e :: ExitCode) -> throwIO e,
             Handler \(e :: SomeException) -> logAndExit . toText . displayException $ e
           ]
      where
        logAndExit e = do
          usingReaderT (simpleLogger LevelError) $ logError e
          exitFailure

defaultDataDir :: IO (Path Abs Dir)
defaultDataDir = parseRelDir (toString Meta.name) <&> Just >>= getXdgDir XdgCache

data MCConfig = MCConfig
  { dirConfig :: DirConfig,
    gameDir :: GameDir,
    mcSide :: MCSide,
    mcAuth :: MCAuth,
    httpManager :: Manager,
    javaConfig :: JavaConfig,
    logger :: Logger
  }
  deriving stock (Generic)

data CLI = CLI
  { dataDir :: Maybe (SomeBase Dir),
    verbosity :: LogLevel,
    cmds :: CLICmds
  }
  deriving stock (Show, Generic)

data CLICmds = Launch LaunchOptions | Auth AuthOptions | Curse CurseOptions
  deriving stock (Show, Generic)

data LaunchOptions = LaunchOptions
  { javaBin :: SomeBase File,
    extraJvmArgs :: [String],
    mcSide :: MCSide,
    forgeVersion :: Maybe ModLoader.VersionQuery,
    fabricVersion :: Maybe ModLoader.VersionQuery,
    authenticate :: Bool,
    mcVersion :: Vanilla.MCVersion,
    gameDir :: SomeBase Dir
  }
  deriving stock (Show, Generic)

data AuthOptions = Login LoginOptions | Logout | Verify
  deriving stock (Show, Generic)

data LoginOptions = LoginOptions
  { email :: Text,
    password :: Text
  }
  deriving stock (Show, Generic)

data CurseOptions = Modpacks ModpackOptions | Mods ModOptions
  deriving stock (Show, Generic)

data ModpackOptions
  = ModpackInstall Curse.ModpackInstallOptions
  | ModpackSearchInstall Curse.ModpackSearchInstallOptions
  deriving stock (Show, Generic)

data ModOptions
  = ModUpdates Curse.ModUpdateOptions
  | ModInstall Curse.ModsInstallOptions
  | ModSearchInstall Curse.ModSearchInstallOptions
  | ModDeduplicate Curse.ModDeduplicateOptions
  deriving stock (Show, Generic)

getCLI :: IO CLI
getCLI = OA.execParser $ OA.info (OA.helper <*> ver <*> cliParser) OA.fullDesc
  where
    ver = OA.infoOption (toString verStr) do
      OA.short 'V' <> OA.long "version" <> OA.help "Print version"
      where
        verStr = Meta.version <> " " <> fromMaybe "(dirty)" rev
          where
            rev = $$(envQ "HELLSMACK_REV") :: Maybe Text

    subcommands = OA.subparser . foldMap simpleCommand
    simpleCommand (name, desc, opts) =
      OA.command name $ OA.info (OA.helper <*> opts) $ OA.progDesc desc <> OA.fullDesc
    enumLike t getKey mods =
      t (OA.maybeReader $ inverseMap getKey) $ OA.completeWith allKeys <> mv <> sd <> mods
      where
        allKeys = getKey <$> universe
        mv = OA.metavar $ "<" <> intercalate "|" allKeys <> ">"
        sd = OA.showDefaultWith getKey
    filterAuto f = OA.maybeReader $ mfilter f . readMaybe

    fileAction = OA.action "file"
    dirAction = OA.action "directory"
    pathLike f a t mods = t (OA.str <&> f >>= either (fail . displayException) pure) $ a <> mods
    filePath = pathLike parseSomeFile fileAction
    dirPath = pathLike parseSomeDir dirAction
    mcSideLike t = enumLike t \case MCClient -> "client"; MCServer -> "server"
    mcVersionLike t mods =
      fmap Vanilla.MCVersion . t $
        OA.metavar "MINECRAFT-VERSION"
          <> mcVersionCompleter
          <> mods
          <> OA.help "Minecraft version, like 1.7.10 or 1.16-pre8"
    versionLatest, versionRecommended :: String
    specialVersions@(versionLatest, versionRecommended) = ("latest", "recommended")
    versionQuery (name :: Text) opts = optional $ OA.option
      do
        OA.str <&> \case
          fv | fv == versionLatest -> ModLoader.LatestVersion
          fv | fv == versionRecommended -> ModLoader.RecommendedVersion
          fv -> ModLoader.ConcreteVersion $ toText fv
      do opts <> OA.help [i|$name version, or '${versionRecommended}' or '${versionLatest}'|]
    releaseTypeOpt = enumLike
      OA.option
      \case
        Curse.Release -> "release"
        Curse.Beta -> "beta"
        Curse.Alpha -> "alpha"
      do
        OA.short 't'
          <> OA.long "max-release-type"
          <> OA.value Curse.Beta
          <> OA.help "most unstable release type to display"
    outDirOpt = dirPath OA.option do
      OA.short 'o' <> OA.long "out-dir" <> OA.help "Output directory" <> OA.value (Rel [reldir|.|])

    completerFromFile fp s f =
      OA.completer . OA.listIOCompleter $
        (s <>) <$> handleAnyDeep (const $ pure []) do
          fp <- runReaderT fp =<< newDirConfig =<< defaultDataDir
          f <$> do decodeJSON =<< readFileLBS (toFilePath fp)
    mcVersionCompleter = completerFromFile
      Vanilla.allVersionsManifestPath
      []
      \(avm :: Vanilla.AllVersionsManifest) ->
        avm ^.. #versions . each . #id . to unMCVersion . unpacked
    forgeVersionCompleter = completerFromFile
      Forge.allVersionsManifestPath
      do specialVersions ^.. each
      \(v :: Value) -> v ^.. members . values . _String . unpacked
    fabricVersionCompleter = completerFromFile
      Fabric.allVersionsManifestPath
      do specialVersions ^.. each
      \(v :: Value) -> v ^.. values . key "version" . _String . unpacked

    cliParser = do
      dataDir <-
        optional $ dirPath OA.option do
          OA.short 'd'
            <> OA.long "data-dir"
            <> OA.help "Directory for assets, libraries etc. (cached)"
      verbosity <- enumLike
        OA.option
        \case
          LevelTrace -> "trace"
          LevelDebug -> "debug"
          LevelInfo -> "info"
          LevelWarn -> "warn"
          LevelError -> "error"
        do OA.short 'v' <> OA.long "verbosity" <> OA.value LevelInfo <> OA.help "Verbosity level"
      cmds <-
        subcommands
          [ ("launch", "Minecraft (Forge) launcher", launchOptions),
            ("auth", "Log in with a Minecraft account", authOptions),
            ("curse", "CurseForge related utilities", curseOptions)
          ]
      pure CLI {..}

    launchOptions = do
      javaBin <- filePath OA.option do
        OA.short 'j'
          <> OA.long "java"
          <> OA.showDefault
          <> OA.value (Rel [relfile|java|])
          <> OA.help "Java path"
      extraJvmArgs <-
        let a1 = many $ OA.strOption do
              OA.short 'J' <> OA.help "add one JVM argument" <> OA.metavar "ARG..."
            a2 = fmap (fmap toString . words) . OA.strOption $ do
              OA.long "jvm-extra-args" <> OA.help "add JVM arguments" <> OA.metavar "ARGS" <> OA.value ""
         in (<>) <$> a1 <*> a2
      forgeVersion <- versionQuery "Forge" $ OA.short 'f' <> OA.long "forge" <> forgeVersionCompleter
      fabricVersion <- versionQuery "Fabric" $ OA.long "fabric" <> fabricVersionCompleter
      authenticate <- OA.switch do
        OA.short 'a' <> OA.long "authenticate" <> OA.help "Authenticate (online mode)"
      mcSide <- mcSideLike OA.argument do
        OA.help "Launch client or server"
      mcVersion <- mcVersionLike OA.strArgument mempty
      gameDir <- dirPath OA.argument do
        OA.metavar "GAME-DIR" <> OA.help "Directory for worlds, options, mods etc."
      pure $ Launch LaunchOptions {..}

    authOptions =
      Auth
        <$> subcommands
          [ ( "login",
              "Login to Minecraft via Yggdrasil",
              do
                email <- OA.strArgument $ OA.metavar "EMAIL"
                password <- OA.strArgument $ OA.metavar "PASSWORD"
                pure $ Login LoginOptions {..}
            ),
            ("logout", "Logout", pure Logout),
            ("verify", "Verify authentication status", pure Verify)
          ]

    curseOptions =
      Curse
        <$> subcommands
          [ ( "modpacks",
              "Utilities for CurseForge modpacks",
              Modpacks
                <$> subcommands
                  [ ( "install",
                      "Install a CurseForge modpack by file ID",
                      do
                        outDir <- outDirOpt
                        mcSide <- mcSideLike OA.argument do
                          OA.help "Download client or server modpack"
                        fileId <- OA.argument (Curse.AddonFileId <$> OA.auto) do
                          OA.metavar "FILE-ID" <> OA.help "Curse file ID of the modpack"
                        pure $ ModpackInstall Curse.ModpackInstallOptions {..}
                    ),
                    ( "search-install",
                      "Search and install a modpack",
                      do
                        numModpacks <- OA.option (filterAuto (> 0)) do
                          OA.short 'm'
                            <> OA.long "num-modpacks"
                            <> OA.value 8
                            <> OA.showDefault
                            <> OA.help "number of modpacks to show"
                        numFiles <- OA.option (filterAuto (> 0)) do
                          OA.short 'f'
                            <> OA.long "num-files"
                            <> OA.value 8
                            <> OA.showDefault
                            <> OA.help "number of concrete modpack versions to show"
                        maxReleaseType <- releaseTypeOpt
                        outDir <- outDirOpt
                        mcVersion <- optional $ mcVersionLike OA.strOption do
                          OA.short 'v' <> OA.long "mc-version"
                        mcSide <- mcSideLike OA.argument do
                          OA.help "Download client or server modpack"
                        modpackName <- OA.strArgument do
                          OA.metavar "MODPACK-NAME" <> OA.help "Modpack name to search for"
                        pure $ ModpackSearchInstall Curse.ModpackSearchInstallOptions {..}
                    )
                  ]
            ),
            ( "mods",
              "Utilities for CurseForge mods",
              Mods
                <$> subcommands
                  [ ( "install",
                      "Install mods by file IDs",
                      do
                        outDir <- outDirOpt
                        fileIds <- some $ OA.argument (Curse.AddonFileId <$> OA.auto) do
                          OA.metavar "FILE-IDs..." <> OA.help "Curse file IDs of mods"
                        pure $ ModInstall Curse.ModsInstallOptions {..}
                    ),
                    ( "search-install",
                      "Search and install a mod",
                      do
                        numMods <- OA.option (filterAuto (> 0)) do
                          OA.short 'm'
                            <> OA.long "num-mods"
                            <> OA.value 8
                            <> OA.showDefault
                            <> OA.help "number of mods to show"
                        numFiles <- OA.option (filterAuto (> 0)) do
                          OA.short 'f'
                            <> OA.long "num-files"
                            <> OA.value 8
                            <> OA.showDefault
                            <> OA.help "number of mod files to show"
                        maxReleaseType <- releaseTypeOpt
                        modLoader <- enumLike
                          OA.option
                          \case
                            Curse.ForgeModLoader -> "forge"
                            Curse.FabricModLoader -> "fabric"
                          do
                            OA.short 'l'
                              <> OA.long "mod-loader"
                              <> OA.value Curse.ForgeModLoader
                              <> OA.help "mod loader to use"
                        outDir <- outDirOpt
                        installDependencies <-
                          not <$> OA.switch do
                            OA.short 'd'
                              <> OA.long "ignore-dependencies"
                              <> OA.help "whether to ignore mod dependencies"
                        mcVersion <- mcVersionLike OA.strArgument mempty
                        modName <- OA.strArgument do
                          OA.metavar "MOD-NAME" <> OA.help "Mod name to search for"
                        pure $ ModSearchInstall Curse.ModSearchInstallOptions {..}
                    ),
                    ( "update",
                      "Update mods to a newer version",
                      do
                        outDir <- optional $ dirPath OA.option do
                          OA.short 'o' <> OA.long "out" <> OA.help "Output directory"
                        keepOld <- OA.switch do
                          OA.short 'k' <> OA.long "keep-old" <> OA.help "Keep old mod files"
                        maxReleaseType <- releaseTypeOpt
                        mcVersion <- optional $ mcVersionLike OA.strOption do
                          OA.short 'v' <> OA.long "mc-version"
                        inputs <- some $ OA.strArgument do
                          OA.metavar "INPUTS..."
                            <> fileAction
                            <> OA.help "Input files and/or directories"
                        pure $ ModUpdates Curse.ModUpdateOptions {..}
                    ),
                    ( "deduplicate",
                      "Deduplicate mods by deleting older versions",
                      do
                        modDir <- dirPath OA.argument do
                          OA.metavar "MOD-DIR" <> OA.help "Mod directory"
                        pure $ ModDeduplicate Curse.ModDeduplicateOptions {..}
                    )
                  ]
            )
          ]
