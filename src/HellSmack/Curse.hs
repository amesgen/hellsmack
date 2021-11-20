module HellSmack.Curse
  ( -- * Modpacks

    -- ** Download
    ModpackInstallOptions (..),
    downloadFullModpack,

    -- ** Search + Install
    ModpackSearchInstallOptions (..),
    searchInstallModpack,

    -- * Mods

    -- ** Update
    ModUpdateOptions (..),
    ModLoader (..),
    updateMods,

    -- ** Install
    ModsInstallOptions (..),
    installMods,

    -- ** Search + Install
    ModSearchInstallOptions (..),
    searchInstallMod,

    -- ** Deduplicate
    ModDeduplicateOptions (..),
    deduplicateMods,
  )
where

import BroadcastChan.Conduit qualified as BCC
import Codec.Archive.Zip
import Colourista.Pure qualified as C
import Conduit hiding (ReleaseType)
import Data.Align
import Data.List (isInfixOf)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Lens
import GHC.Conc (getNumCapabilities)
import HellSmack.Curse.API
import HellSmack.Logging
import HellSmack.Util hiding (extension)
import Relude.Extra.Group
import Text.Layout.Table qualified as TL
import Text.Layout.Table.Cell.Formatted qualified as TL
import UnliftIO.Directory qualified as FP
import UnliftIO.Temporary

data ModpackManifest = ModpackManifest
  { minecraft :: MinecraftInfo,
    name :: Text,
    version :: Text,
    author :: Text,
    overrides :: FilePath,
    files :: [ModDownload]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data MinecraftInfo = MinecraftInfo
  { version :: MCVersion,
    modLoaders :: [ModLoaderInfo]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data ModDownload = ModDownload
  { projectID :: AddonId,
    fileID :: AddonFileId,
    required :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data ModLoaderInfo = ModLoaderInfo
  { id :: Text,
    primary :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data ModpackInstallOptions = ModpackInstallOptions
  { mcSide :: MCSide,
    fileId :: AddonFileId,
    outDir :: SomeBase Dir
  }
  deriving stock (Show, Generic)

downloadFullModpack ::
  (MonadUnliftIO m, MRHasAll r [Manager, Logger] m) =>
  ModpackInstallOptions ->
  m ()
downloadFullModpack ModpackInstallOptions {..} = do
  outDir <- makeSomeAbsolute outDir
  logInfo "starting modpack download"
  withModpackMetadata mcSide fileId \metaFile ->
    case mcSide of
      MCClient -> do
        logInfo "parsing modpack manifest"
        manifest :: ModpackManifest <- withArchive (toFilePath metaFile) do
          es <- mkEntrySelector "manifest.json"
          getEntry es >>= decodeJSON . toLazy
        let modpackVersion = C.formatWith [C.bold] $ manifest ^. #version
            mcVersion :: Text = C.formatWith [C.bold] $ show $ manifest ^. #minecraft . #version
        logInfo [i|modpack version: $modpackVersion, MC version: $mcVersion|]
        forOf_
          do #minecraft . #modLoaders . each . filtered (^. #primary)
          manifest
          \ModLoaderInfo {id} -> logInfo $ [i|mod loader: ${}|] $ C.formatWith [C.bold] id

        let (optionalFileIds, requiredFileIds) =
              manifest ^. #files & partitionWith \f ->
                (chosen %~ (^. #fileID)) . bool Left Right (f ^. #required) $ f

        logInfo $ [i|fetching metadata of $show mods|] $ length requiredFileIds
        files <- getAddonFilesByFileIds requiredFileIds <&> (^.. each . folded)

        let modDir = outDir </> [reldir|mods|]
        ensureDir modDir
        (unknownMods, oldFiles) <- findInCurseDB [toFilePath modDir]
        let oldModsByFP = oldFiles <&> \(_, af, p) -> (af ^. #packageFingerprint, (af, p))
            newModsByFP = files & fmapToFst (^. #packageFingerprint)
            (unzip -> (_, knownOldMods), trulyNewMods, sameMods) =
              partitionThese . M.elems $ M.fromList oldModsByFP `align` M.fromList newModsByFP
            deletableOldFiles = unknownMods <> knownOldMods
        for_ sameMods \((oldFile, toFilePath -> p), newFile) ->
          when (oldFile ^. #id /= newFile ^. #id) $ throwString [i|internal ID conflict for mod $p|]

        whenNotNull sameMods $ logInfo . [i|$show mods already present|] . length

        case trulyNewMods of
          [] -> logInfo "no new mods need to be downloaded"
          _ -> do
            let modSize = showBytes' $ sumOf (each . #fileLength) trulyNewMods
                modCount = length trulyNewMods
            logInfo [i|downloading ${show modCount} mods ($modSize)|]
            stepWise (withGenericProgress modCount) \step ->
              forConcurrentlyNetwork_ trulyNewMods \a -> step $ downloadAddonFile a modDir HideProgress

        whenNotNull deletableOldFiles \_ -> do
          logInfo $ [i|deleting $show stale mod files|] (length deletableOldFiles)
          for_ deletableOldFiles \p -> do
            logTrace $ [i| - ${}|] $ toFilePath p
            removeFile p

        whenNotNull optionalFileIds \_ -> do
          urls <- getAddonFilesByFileIds optionalFileIds <&> (^.. each . folded . #downloadUrl)
          logInfo "optional mods available:"
          for_ urls \url -> logInfo [i| - $url|]

        logInfo "extracting and copying other game files"
        extractFromZip metaFile (manifest ^. #overrides) outDir
      MCServer -> do
        logInfo "extracting modpack"
        extractFromZip metaFile "" outDir

withModpackMetadata ::
  (MonadUnliftIO m, MRHasAll r [Manager, Logger] m) =>
  MCSide ->
  AddonFileId ->
  (Path Abs File -> m a) ->
  m a
withModpackMetadata mcSide afid cont = do
  (addon, file) <-
    getAddonFilesByFileIds [afid] <&> (^@? ifolded) >>= \case
      Nothing -> throwString "invalid modpack file id: project does not exist"
      Just (addonId, file :| _) -> do
        file <- do
          when (file ^. #categorySectionPackageType /= Just minecraftModpackPackageType) $
            throwString "project of file id is not a modpack"
          when (file ^. #isServerPack == Just True) $
            throwString "project of file id is a server pack"
          case mcSide of
            MCClient -> pure file
            MCServer -> do
              serverId <-
                file ^. #serverPackFileId & maybe (throwString "no server pack available") pure
              file <-
                getAddonFilesByFileIds [serverId] <&> (^? each . folded)
                  >>= maybe (throwString "server pack not found") pure
              when (file ^. #isServerPack /= Just True) $
                throwString "expected a server pack from curse"
              pure file
        addon <- getAddon addonId
        when (addon ^. #gameId /= minecraftGameId) $
          throwString "not a Minecraft project"
        pure (addon, file)
  let modpackName = C.formatWith [C.green, C.bold] $ addon ^. #name
      modpackFileName = C.formatWith [C.bold] $ file ^. #displayName
  logInfo [i|found modpack $modpackName: $modpackFileName|]
  logInfo $
    "downloading " <> case mcSide of
      MCClient -> "general metadata and non-mod files"
      MCServer -> "server pack"
  withSystemTempDirectory "modpack" \dir -> do
    dir <- reThrow $ parseAbsDir dir
    downloadAddonFile file dir ShowProgress
    cont (dir </> file ^. #fileName)

data ModpackSearchInstallOptions = ModpackSearchInstallOptions
  { mcVersion :: Maybe MCVersion,
    outDir :: SomeBase Dir,
    modpackName :: Text,
    numModpacks :: Word32,
    numFiles :: Word32,
    maxReleaseType :: ReleaseType,
    mcSide :: MCSide
  }
  deriving stock (Show, Generic)

searchInstallModpack ::
  (MonadUnliftIO m, MRHasAll r [Manager, Logger] m) =>
  ModpackSearchInstallOptions ->
  m ()
searchInstallModpack ModpackSearchInstallOptions {..} = do
  logInfo "searching modpack metadata"
  addons <-
    searchAddons
      SearchCriteria
        { gameId = minecraftGameId,
          gameVersion = coerce mcVersion ?: "",
          categorySectionId = minecraftModpacksSection,
          categoryId = catchAllCategory,
          sortMethod = ByPopularity,
          sortDescending = True,
          index = 0,
          pageSize = numModpacks,
          searchFilter = modpackName
        }
  addon <- case addons & take (fromIntegral numModpacks) & nonEmpty of
    Nothing -> Nothing <$ logWarn "no modpacks found"
    Just addons ->
      Just <$> selectViaTable addons ["name", "authors", "download count"] \a ->
        [ formattedVia C.green . toString $ a ^. #name,
          TL.plain . intercalate ", " $ a ^.. #authors . each . #name . unpacked,
          TL.plain . show @_ @Int . round $ a ^. #downloadCount
        ]
  whenJust addon \addon -> do
    files <-
      getAddonFilesByAddonId (addon ^. #id)
        <&> sortOn (^. #fileDate . to Down) . filter \f ->
          coerce (toList mcVersion) `isInfixOf` (f ^. #gameVersion)
            && f ^. #releaseType <= maxReleaseType
    file <- case files & take (fromIntegral numFiles) & nonEmpty of
      Nothing -> logWarn "no modpack versions found" $> Nothing
      Just files ->
        Just <$> selectViaTable files ["name", "release date", "type"] \f ->
          [ formattedVia C.green . toString $ f ^. #displayName,
            TL.plain . show $ f ^. #fileDate,
            showReleaseType f
          ]
    whenJust file \AddonFile {id = fileId, displayName = modpackFileName} -> do
      let mfn = C.formatWith [C.green, C.bold] modpackFileName
      logInfo [i|selected $mfn|]
      downloadFullModpack ModpackInstallOptions {..}

extractFromZip :: MonadUnliftIO m => Path Abs File -> FilePath -> Path Abs Dir -> m ()
extractFromZip zipFile zipDir' targetDir = liftIO do
  entries <- withArchive (toFilePath zipFile) $ getEntries <&> (^.. ifolded . asIndex)
  stepWise (withGenericProgress (length entries)) \step ->
    withArchive (toFilePath zipFile) $
      for_ entries \es ->
        forOf_ (to unEntrySelector . prefixed zipDir) es \relPath -> step do
          relPath <- parseRelFile relPath
          let targetFile = targetDir </> relPath
          ensureDir $ parent targetFile
          saveEntry es (toFilePath targetFile)
  where
    zipDir = case zipDir' ^? _last of
      Just c | c /= '/' -> zipDir' <> "/"
      _ -> zipDir'

fingerprintFiles ::
  (MonadUnliftIO m, MonadResource m) =>
  ConduitT (Path Abs File) (Fingerprint, Path Abs File) m ()
fingerprintFiles = do
  numCap <- liftIO getNumCapabilities
  BCC.parMapM (BCC.Simple BCC.Terminate) numCap $ traverseToFst fingerprintFile

findModFiles :: MonadResource m => ConduitT FilePath (Path Abs File) m ()
findModFiles =
  mapMC FP.makeAbsolute
    .| awaitForever expandDirs
    .| mapMC (reThrow . parseAbsFile)
    .| filterC do (== Just ".jar") . fileExtension
  where
    expandDirs fp =
      FP.doesFileExist fp >>= \case
        True -> yield fp
        False -> sourceDirectory fp

findInCurseDB ::
  (MonadUnliftIO m, MRHasAll r [Manager, Logger] m) =>
  [FilePath] ->
  m ([Path Abs File], [(AddonId, AddonFile, Path Abs File)])
findInCurseDB inputs = do
  logInfo "fingerprinting mod files"
  fps <- runConduitRes $ yieldMany inputs .| findModFiles .| fingerprintFiles .| sinkList
  logInfo "querying CurseForge mod database"
  fpms <-
    M.fromList . (^.. #exactMatches . each . to ((^. #file . #packageFingerprint) &&& identity))
      <$> getFingerprintMatches (fst <$> fps)
  let result@(unknown, _) =
        fps & partitionWith \(fp, path) -> case fpms ^? ix fp of
          Just FingerprintMatch {..} -> Right (id, file, path)
          Nothing -> Left path
  unless (null unknown) do
    logWarn $ [i|these ${show} files are not present in the CurseForge mod database:|] $ length unknown
    for_ unknown $ logWarn . [i| - ${}|] . toFilePath
  pure result

data ModLoader = ForgeModLoader | FabricModLoader
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)

detectModLoader :: AddonFile -> ModLoader
detectModLoader af
  | "Fabric" `elem` (af ^. #gameVersion) = FabricModLoader
  | otherwise = ForgeModLoader

data ModUpdateOptions = ModUpdateOptions
  { outDir :: Maybe (SomeBase Dir),
    keepOld :: Bool,
    maxReleaseType :: ReleaseType,
    mcVersion :: Maybe MCVersion,
    inputs :: [FilePath]
  }
  deriving stock (Show, Generic)

updateMods :: (MonadUnliftIO m, MRHasAll r [Manager, Logger] m) => ModUpdateOptions -> m ()
updateMods ModUpdateOptions {..} = do
  outDir <- outDir & _Just %%~ makeSomeAbsolute
  (_, updatable) <- findInCurseDB inputs
  unless (null updatable) do
    logInfo $ [i|fetching update info for $show mods|] $ length updatable
    let isNewerFile file f =
          f ^. #fileDate > file ^. #fileDate
            && case mcVersion of
              Just mcv -> coerce mcv `elem` (f ^. #gameVersion)
              Nothing ->
                file ^. #gameVersion
                  & filter isMCVersionString
                  & any (`elem` do f ^. #gameVersion)
            && detectModLoader f == detectModLoader file
            && f ^. #id /= file ^. #id
            && f ^. #releaseType <= maxReleaseType
    (sortOn $ has (_4 . _Just) &&& (^. _2 . #name) -> updatable) <-
      forConcurrentlyNetwork updatable \(id, file, path) -> do
        addon <- getAddon id
        newerFiles <-
          getAddonFilesByAddonId id
            <&> filter do isNewerFile file
            <&> sortOn (^. #fileDate . to Down)
        pure (path, addon, file, nonEmpty newerFiles)
    whenJust outDir ensureDir
    for_ updatable \(path, addon, file, newerFiles) -> do
      let addonName c a = C.formatWith c $ a ^. #name
          fileName c f = C.formatWith c $ f ^. #displayName
      case newerFiles of
        Nothing -> logInfo $ [i|no updates for ${}|] $ addonName [C.bold] addon
        Just newerFiles -> do
          let an = addonName [C.bold, C.green] addon
              fn = fileName [C.bold] file
          logInfo [i|updating $an from $fn...|]
          newFile <- selectViaTable
            (pure file <> newerFiles)
            ["name", "release date", "type"]
            \f ->
              [ if f ^. #id == file ^. #id
                  then formattedVia C.yellow "current version"
                  else formattedVia C.bold . toString $ f ^. #displayName,
                TL.plain $ f ^. #fileDate . to show,
                showReleaseType f
              ]
          if newFile ^. #id == file ^. #id
            then logInfo "staying on current version"
            else do
              logInfo $ [i|updating to ${}|] $ fileName [C.green, C.bold] newFile
              downloadAddonFile newFile (outDir ?: parent path) ShowProgress
              unless keepOld $ removeFile path
  where
    -- TODO improve detection
    isMCVersionString = (`notElem` ["Forge", "Fabric"])

showReleaseType :: AddonFile -> TL.Formatted String
showReleaseType f = case f ^. #releaseType of
  Release -> formattedVia C.green "RELEASE"
  Beta -> formattedVia C.blue "BETA"
  Alpha -> formattedVia C.red "ALPHA"

data ModsInstallOptions = ModsInstallOptions
  { fileIds :: [AddonFileId],
    outDir :: SomeBase Dir
  }
  deriving stock (Show, Generic)

installMods :: (MonadUnliftIO m, MRHasAll r [Manager, Logger] m) => ModsInstallOptions -> m ()
installMods ModsInstallOptions {..} = do
  outDir <- makeSomeAbsolute outDir
  logInfo "fetching mod metadata"
  addonFiles <- getAddonFilesByFileIds fileIds
  whenNotNull
    do toList $ S.fromList fileIds S.\\ S.fromList (addonFiles ^.. each . to head . #id)
    \(coerce @_ @[Int] . toList -> unknownFileIds) ->
      throwString $ [i|unknown file IDs: ${}|] $ T.intercalate ", " $ show <$> unknownFileIds
  addons <- getAddons (addonFiles ^.. ifolded . asIndex) <&> M.fromList . fmapToFst (^. #id)
  ensureDir outDir
  addonFiles <- forM (itoList addonFiles) \(aid, file :| _) -> do
    addon <- addons ^? ix aid & maybe (throwString "internal error") pure
    when (addon ^. #gameId /= minecraftGameId) $
      throwString "not a minecraft addon"
    when (file ^. #categorySectionPackageType /= Just minecraftModPackageType) $
      throwString "not a mod project"
    pure (addon, file)
  let tableLines =
        TL.gridLines [TL.def, TL.def] $
          addonFiles <&> \(addon, file) ->
            [ formattedVia (C.bold <> C.green) . toString $ addon ^. #name,
              formattedVia C.bold . toString $ file ^. #displayName
            ]
  logInfo $ [i|downloading $show mods|] $ length addonFiles
  forConcurrentlyNetwork_ (addonFiles `zip` tableLines) \((_, file), line) -> do
    logInfo $ toText line
    downloadAddonFile file outDir HideProgress

data ModSearchInstallOptions = ModSearchInstallOptions
  { mcVersion :: MCVersion,
    outDir :: SomeBase Dir,
    modName :: Text,
    numMods :: Word32,
    numFiles :: Word32,
    maxReleaseType :: ReleaseType,
    installDependencies :: Bool,
    modLoader :: ModLoader
  }
  deriving stock (Show, Generic)

searchInstallMod ::
  forall m r.
  (MonadUnliftIO m, MRHasAll r [Manager, Logger] m) =>
  ModSearchInstallOptions ->
  m ()
searchInstallMod ModSearchInstallOptions {..} = do
  outDir <- makeSomeAbsolute outDir
  logInfo "searching mod metadata"
  addons <-
    searchAddons
      SearchCriteria
        { gameId = minecraftGameId,
          gameVersion = coerce mcVersion,
          categorySectionId = minecraftModsSection,
          categoryId = case modLoader of
            -- there is no category for Forge, only for Fabric
            ForgeModLoader -> catchAllCategory
            FabricModLoader -> fabricModsCategory,
          sortMethod = ByPopularity,
          sortDescending = True,
          index = 0,
          pageSize = numMods,
          searchFilter = modName
        }
  addon <- case addons & take (fromIntegral numMods) & nonEmpty of
    Nothing -> Nothing <$ logWarn "no mods found"
    Just addons ->
      Just <$> selectViaTable addons ["name", "authors", "download count"] \a ->
        [ formattedVia C.green . toString $ a ^. #name,
          TL.plain . intercalate ", " $ a ^.. #authors . each . #name . unpacked,
          TL.plain . show @_ @Int . round $ a ^. #downloadCount
        ]
  whenJust addon \a@Addon {name = addonName} -> do
    logInfo $ [i|selected ${}|] $ C.formatWith [C.green, C.bold] addonName
    whenJustM (selectAddonFile a) \file -> do
      ensureDir outDir
      downloadAddonFile file outDir ShowProgress

      when installDependencies do
        logInfo [i|fetching dependency metadata|]
        deps <- findDeps requiredDependencyType file
        whenNotNull deps \deps -> do
          logInfo $ [i|downloading ${} dependencies|] $ C.formatWith [C.green, C.bold] addonName
          stepWise (withGenericProgress (length deps)) \step ->
            forConcurrentlyNetwork_ deps \file ->
              step $ downloadAddonFile file outDir HideProgress
  where
    selectAddonFile a = do
      files <-
        getAddonFilesByAddonId (a ^. #id)
          <&> sortOn (^. #fileDate . to Down) . filter \f ->
            coerce mcVersion `elem` (f ^. #gameVersion)
              && detectModLoader f == modLoader
              && f ^. #releaseType <= maxReleaseType
      file <- case files & take (fromIntegral numFiles) & nonEmpty of
        Nothing -> logWarn "no mod files found" $> Nothing
        Just files ->
          Just <$> selectViaTable files ["name", "release date", "type", "file size"] \f ->
            [ formattedVia C.green . toString $ f ^. #displayName,
              TL.plain . show $ f ^. #fileDate,
              showReleaseType f,
              TL.plain . toString . showBytes' $ f ^. #fileLength
            ]
      whenJust file \AddonFile {displayName = modFileName} ->
        logInfo $ [i|selected ${}|] $ C.formatWith [C.green, C.bold] modFileName
      pure file

    findDeps dt =
      evaluatingStateT S.empty . fix \go file -> do
        aids <- get
        let deps =
              file ^. #dependencies
                & filter do \d -> d ^. #dependencyType == dt
                & filter do \d -> (d ^. #addonId) `S.notMember` aids
            depsAids = deps ^.. each . #addonId
        modify (<> S.fromList depsAids)
        getAddons depsAids <&> (deps `zip`) >>= foldMapM \(dep, addon) -> do
          logInfo $ [i|processing dependency ${}|] $ C.formatWith [C.bold] $ addon ^. #name
          file <- case dep ^. #fileId of
            Just fileId -> Just <$> getAddonFile (dep ^. #addonId) fileId
            Nothing -> lift $ selectAddonFile addon
          file & foldMapM \file -> (file :) <$> go file

downloadAddonFile :: HasManagerIO r m => AddonFile -> Path Abs Dir -> ProgressOption -> m ()
downloadAddonFile AddonFile {..} outDir =
  downloadToFile downloadUrl (outDir </> fileName)

newtype ModDeduplicateOptions = ModDeduplicateOptions
  { modDir :: SomeBase Dir
  }
  deriving stock (Show, Generic)

deduplicateMods ::
  (MonadUnliftIO m, MRHasAll r [Manager, Logger] m) =>
  ModDeduplicateOptions ->
  m ()
deduplicateMods ModDeduplicateOptions {..} = do
  modDir <- makeSomeAbsolute modDir
  (_, updatable) <- findInCurseDB [toFilePath modDir]
  updatable
    & groupBy @_ @(Map _ _) (^. _1)
    <&> NE.sortWith (^. _2 . #fileDate)
    & (^.. each . to init . each . _3)
    & \case
      [] -> logInfo [i|no duplicated mods found|]
      toBeRemoved -> do
        logInfo $ [i|removing $show old mods:|] $ length toBeRemoved
        for_ toBeRemoved $ logInfo . [i| - ${}|] . toFilePath
        promptBool "remove" >>= \case
          True -> logInfo "removing old mods" *> for_ toBeRemoved removeFile
          False -> logInfo "keeping old mods"
