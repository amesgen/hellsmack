module HellSmack.Curse.API
  ( -- * Types
    AddonId (..),
    AddonAttachmentId (..),
    AddonAuthorId (..),
    AddonFileId (..),
    GameId (..),
    CategoryId (..),
    CategorySectionId (..),
    Fingerprint (..),
    Addon (..),
    AddonFile (..),
    AddonAttachment (..),
    AddonAuthor (..),
    AddonCategory (..),
    AttachmentStatus (..),
    ProjectStatus (..),
    PackageType (..),
    ReleaseType (..),
    CategorySection (..),
    LatestAddonFile (..),
    AddonFileDependency (..),
    DependencyType (..),
    FingerprintMatchResult (..),
    FingerprintMatch (..),
    SearchCriteria (..),
    AddonSortMethod (..),
    Game (..),

    -- * Constants (mainly Minecraft-related)
    minecraftGameId,

    -- ** CategorySectionId
    catchAllSection,
    minecraftModsSection,
    minecraftModpacksSection,

    -- ** CategoryId
    catchAllCategory,
    fabricModsCategory,

    -- ** PackageType
    minecraftModPackageType,
    minecraftModpackPackageType,

    -- ** DependencyType
    requiredDependencyType,
    optionalDependencyType,

    -- * Fetching
    getAddon,
    getAddons,
    searchAddons,
    getAddonFile,
    getAddonFilesByAddonId,
    getAddonFilesByFileIds,
    getFingerprintMatches,
    getGame,
    getGames,
    getCategories,

    -- * Fingerprinting
    fingerprintFile,
  )
where

import Conduit hiding (ReleaseType)
import Data.Conduit.Serialization.Binary
import Data.Time
import HellSmack.Util
import Network.HTTP.Client
import System.Random (randomRIO)
import UnliftIO.IO

newtype AddonId = AddonId Int
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, FromJSONKey, ToJSON)

newtype AddonAttachmentId = AddonAttachmentId Int
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON)

newtype AddonAuthorId = AddonAuthorId Int
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON)

newtype AddonFileId = AddonFileId Int
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype GameId = GameId Int
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON)

newtype CategoryId = CategoryId Int
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON)

newtype CategorySectionId = CategorySectionId Int
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON)

newtype Fingerprint = Fingerprint Word32
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

data Addon = Addon
  { id :: AddonId,
    name :: Text,
    authors :: [AddonAuthor],
    attachments :: [AddonAttachment],
    gameId :: GameId,
    gameName :: Text,
    summary :: Text,
    downloadCount :: Double,
    categories :: [AddonCategory],
    status :: ProjectStatus,
    primaryCategoryId :: CategoryId,
    slug :: Text,
    gameSlug :: Text,
    latestFiles :: [AddonFile],
    isFeatured :: Bool,
    popularityScore :: Double,
    gamePopularityRank :: Int,
    portalName :: Text,
    dateModified :: UTCTime,
    dateCreated :: UTCTime,
    dateReleased :: UTCTime,
    isAvailable :: Bool,
    isExperimental :: Bool,
    primaryLanguage :: String
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON) via CustomJSONLabel '[Rename "isExperimental" "isExperiemental"] Addon

data AddonAttachment = AddonAttachment
  { id :: AddonAttachmentId,
    projectId :: AddonId,
    description :: Text,
    isDefault :: Bool,
    status :: AttachmentStatus,
    thumbnailUrl :: Text,
    title :: Text,
    url :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data AddonAuthor = AddonAuthor
  { id :: AddonAuthorId,
    name :: Text,
    url :: Text,
    projectId :: AddonId,
    userId :: Int,
    twitchId :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data AddonFile = AddonFile
  { id :: AddonFileId,
    displayName :: Text,
    fileName :: Path Rel File,
    fileDate :: UTCTime,
    fileLength :: Word64,
    fileStatus :: ProjectStatus,
    releaseType :: ReleaseType,
    downloadUrl :: Text,
    isAlternate :: Bool,
    exposeAsAlternative :: Maybe Bool,
    alternateFileId :: AddonFileId,
    dependencies :: [AddonFileDependency],
    isAvailable :: Bool,
    isServerPack :: Maybe Bool,
    serverPackFileId :: Maybe AddonFileId,
    parentProjectFileId :: Maybe AddonFileId,
    modules :: [AddonFileModule],
    packageFingerprint :: Fingerprint,
    projectStatus :: Maybe ProjectStatus,
    gameId :: Maybe GameId,
    gameVersion :: [Text],
    categorySectionPackageType :: Maybe PackageType
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data AddonCategory = AddonCategory
  { categoryId :: CategoryId,
    name :: Text,
    gameId :: GameId,
    parentId :: Maybe CategoryId,
    projectId :: AddonId,
    rootId :: Maybe CategoryId,
    url :: Text,
    avatarUrl :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data AttachmentStatus
  = Normal
  | Deleted
  | Uploaded
  | Banned
  | PendingModeration
  deriving stock (Show, Eq, Generic, Enum, Bounded)

instance FromJSON AttachmentStatus where
  parseJSON =
    parseJSON @Int >=> \case
      1 -> pure Normal
      2 -> pure Deleted
      3 -> pure Uploaded
      4 -> pure Banned
      5 -> pure PendingModeration
      _ -> fail "invalid AttachmentStatus"

-- explicitly list some possible values?
newtype ProjectStatus = ProjectStatus Int
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON)

newtype PackageType = PackageType Int
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON)

data ReleaseType = Release | Beta | Alpha
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)

instance FromJSON ReleaseType where
  parseJSON =
    parseJSON @Int >=> \case
      1 -> pure Release
      2 -> pure Beta
      3 -> pure Alpha
      _ -> fail "invalid ReleaseType"

data CategorySection = CategorySection
  { id :: Int,
    gameCategoryId :: CategorySectionId,
    name :: Text,
    gameId :: GameId,
    packageType :: PackageType
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data LatestAddonFile = LatestAddonFile
  { gameVersion :: Text,
    projectFileId :: AddonFileId,
    projectFileName :: Text,
    fileType :: ReleaseType
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data AddonFileModule = AddonFileModule
  { foldername :: Text,
    fingerprint :: Fingerprint
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data AddonFileDependency = AddonFileDependency
  { addonId :: AddonId,
    fileId :: Maybe AddonFileId,
    dependencyType :: DependencyType
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON) via CustomJSONLabel '[Rename "dependencyType" "type"] AddonFileDependency

newtype DependencyType = DependencyType Int
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON)

data FingerprintMatchResult = FingerprintMatchResult
  { isCacheBuilt :: Bool,
    exactMatches :: [FingerprintMatch],
    unmatchedFingerprints :: [Fingerprint]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data FingerprintMatch = FingerprintMatch
  { id :: AddonId,
    file :: AddonFile,
    latestFiles :: [AddonFile]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data SearchCriteria = SearchCriteria
  { gameId :: GameId,
    gameVersion :: Text,
    categorySectionId :: CategorySectionId,
    categoryId :: CategoryId,
    sortMethod :: AddonSortMethod,
    sortDescending :: Bool,
    index :: Word32,
    pageSize :: Word32,
    searchFilter :: Text
  }
  deriving stock (Show, Eq, Generic)

data AddonSortMethod
  = ByFeatured
  | ByPopularity
  | ByLastUpdated
  | ByName
  | ByAuthor
  | ByTotalDownloads
  | ByCategory
  | ByGameVersion
  deriving stock (Show, Eq, Generic)

-- add missing stuff?
data Game = Game
  { id :: GameId,
    name :: Text,
    slug :: Text,
    supportsAddons :: Bool,
    categorySections :: [CategorySection]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

data Category = Category
  { id :: CategoryId,
    gameId :: GameId,
    name, slug :: Text,
    parentGameCategoryId, rootGameCategoryId :: Maybe CategorySectionId
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

minecraftGameId :: GameId
minecraftGameId = GameId 432

catchAllSection :: CategorySectionId
catchAllSection = CategorySectionId (-1)

minecraftModsSection :: CategorySectionId
minecraftModsSection = CategorySectionId 6

minecraftModpacksSection :: CategorySectionId
minecraftModpacksSection = CategorySectionId 4471

catchAllCategory :: CategoryId
catchAllCategory = CategoryId (-1)

fabricModsCategory :: CategoryId
fabricModsCategory = CategoryId 4780

minecraftModPackageType :: PackageType
minecraftModPackageType = PackageType 6

minecraftModpackPackageType :: PackageType
minecraftModpackPackageType = PackageType 5

requiredDependencyType :: DependencyType
requiredDependencyType = DependencyType 3

optionalDependencyType :: DependencyType
optionalDependencyType = DependencyType 2

baseUrl :: Text
baseUrl = "https://addons-ecs.forgesvc.net/api/v2"

data Method = GET | POST
  deriving stock (Show)

sendJSON ::
  (HasManagerIO r m, FromJSON a, ToJSON b) =>
  Text ->
  Method ->
  [(ByteString, ByteString)] ->
  Maybe b ->
  m a
sendJSON url m qs mb =
  sieh >>= \manager -> liftIO do
    ua <- replicateM 30 $ randomRIO ('a', 'z')
    req <-
      parseUrlThrow [i|$baseUrl/$url|]
        <&> do \req -> req {method = show m}
        <&> case mb of
          Just b -> \req -> req {requestBody = RequestBodyLBS . encode $ b}
          Nothing -> identity
        <&> addRequestHeaders do
          ("User-Agent", encodeUtf8 ua) : case m of
            GET -> []
            POST -> [("Content-Type", "application/json")]
        <&> setQueryString do qs <&> second Just
    httpLbs req manager >>= decodeJSON . responseBody
  where
    addRequestHeaders hs req = req {requestHeaders = hs ++ requestHeaders req}

getJSON :: (HasManagerIO r m, FromJSON a) => Text -> m a
getJSON url = sendJSON url GET [] (Nothing @())

postJSON :: (HasManagerIO r m, FromJSON a, ToJSON b) => Text -> b -> m a
postJSON url = sendJSON url POST [] . Just

getAddon :: (HasManagerIO r m) => AddonId -> m Addon
getAddon (AddonId aid) = getJSON [i|addon/${show aid}|]

getAddons :: (HasManagerIO r m) => [AddonId] -> m [Addon]
getAddons = \case
  [] -> pure []
  aids -> postJSON [i|addon|] aids

searchAddons :: (HasManagerIO r m) => SearchCriteria -> m [Addon]
searchAddons SearchCriteria {..} = sendJSON [i|addon/search|] GET qs (Nothing @())
  where
    qs =
      [ ("gameId", show @_ @Int . coerce $ gameId),
        ("gameVersion", encodeUtf8 gameVersion),
        ("sectionId", show @_ @Int . coerce $ categorySectionId),
        ("categoryId", show @_ @Int . coerce $ categoryId),
        ("sort", encodeUtf8 . T.drop 2 . show $ sortMethod),
        ("isSortDescending", show if sortDescending then 1 :: Int else 0),
        ("index", show index),
        ("pageSize", show pageSize),
        ("searchFilter", encodeUtf8 searchFilter)
      ]

getAddonFile :: (HasManagerIO r m) => AddonId -> AddonFileId -> m AddonFile
getAddonFile (AddonId aid) (AddonFileId fid) = getJSON [i|addon/${show aid}/fid/${show fid}|]

getAddonFilesByAddonId :: (HasManagerIO r m) => AddonId -> m [AddonFile]
getAddonFilesByAddonId (AddonId aid) = getJSON [i|addon/${show aid}/files|]

getAddonFilesByFileIds ::
  (HasManagerIO r m) => [AddonFileId] -> m (Map AddonId (NonEmpty AddonFile))
getAddonFilesByFileIds = postJSON [i|addon/files|]

getFingerprintMatches ::
  (HasManagerIO r m) =>
  [Fingerprint] ->
  m FingerprintMatchResult
getFingerprintMatches = postJSON [i|fingerprint|]

fingerprintFile :: MonadIO m => Path Abs File -> m Fingerprint
fingerprintFile fp = liftIO $ withBinaryFile (toFilePath fp) ReadMode \handle -> do
  let src = sourceHandle handle .| filterCE isNonWSChar
  len <- runConduit $ src .| lengthCE
  hSeek handle AbsoluteSeek 0
  runConduit $ Fingerprint <$> do src .| sinkGet (murmurhash len curseSeed)
  where
    curseSeed = 1
    -- needed as Curse uses "whitespace normalization"™ to hash (non-text!) files
    isNonWSChar b = b /= 9 && b /= 10 && b /= 13 && b /= 32

getGame :: (HasManagerIO r m) => GameId -> m Game
getGame (GameId id) = getJSON [i|game/${show id}|]

getGames ::
  (HasManagerIO r m) =>
  -- | supports addons?
  Bool ->
  m [Game]
getGames supportsAddons = sendJSON [i|game|] GET [("supportsAddons", sa)] (Nothing @())
  where
    sa = if supportsAddons then "true" else "false"

getCategories :: HasManagerIO r m => m [Category]
getCategories = getJSON "category"
