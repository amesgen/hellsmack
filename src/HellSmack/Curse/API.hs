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

    -- * Constants (mainly Minecraft-related)
    minecraftGameId,

    -- ** CategorySectionId
    catchAllSection,
    minecraftModsSection,
    minecraftModpacksSection,

    -- ** CategoryId
    catchAllCategory,

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

    -- * Fingerprinting
    fingerprintFile,
  )
where

import Conduit hiding (ReleaseType)
import Data.Aeson
import Data.Binary.Get
import Data.Bits (shiftR)
import Data.Conduit.Serialization.Binary
import Data.Text qualified as T
import Data.Time
import HellSmack.Util
import Network.HTTP.Client
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

instance FromJSON Addon where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "isExperimental" -> "isExperiemental"
            l -> l
        }

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
  { id :: CategorySectionId,
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

instance FromJSON AddonFileDependency where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "dependencyType" -> "type"
            l -> l
        }

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
    req <-
      parseUrlThrow [i|#{baseUrl}/#{url}|]
        <&> do \req -> req {method = show m}
        <&> case mb of
          Just b -> \req -> req {requestBody = RequestBodyLBS . encode $ b}
          Nothing -> identity
        <&> case m of
          GET -> identity
          POST -> \req ->
            req {requestHeaders = ("Content-Type", "application/json") : requestHeaders req}
        <&> setQueryString do qs <&> second Just
    manager & httpLbs req <&> responseBody <&> eitherDecode' >>= rethrow

getJSON :: (HasManagerIO r m, FromJSON a) => Text -> m a
getJSON url = sendJSON url GET [] (Nothing @())

postJSON :: (HasManagerIO r m, FromJSON a, ToJSON b) => Text -> b -> m a
postJSON url = sendJSON url POST [] . Just

getAddon :: (HasManagerIO r m) => AddonId -> m Addon
getAddon (AddonId aid) = getJSON [i|addon/#{aid}|]

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
getAddonFile (AddonId aid) (AddonFileId fid) = getJSON [i|addon/#{aid}/fid/#{fid}|]

getAddonFilesByAddonId :: (HasManagerIO r m) => AddonId -> m [AddonFile]
getAddonFilesByAddonId (AddonId aid) = getJSON [i|addon/#{aid}/files|]

getAddonFilesByFileIds ::
  (HasManagerIO r m) => [AddonFileId] -> m (Map AddonId (NonEmpty AddonFile))
getAddonFilesByFileIds = postJSON [i|addon/files|]

getFingerprintMatches ::
  (HasManagerIO r m) =>
  [Fingerprint] ->
  m FingerprintMatchResult
getFingerprintMatches = postJSON [i|fingerprint|]

-- | MurmurHash2 (32bit)
murmurhash ::
  -- |  length
  Word32 ->
  -- |  seed
  Word32 ->
  Get Word32
murmurhash !len' !seed = go len' (seed `xor` len')
  where
    go !len !h0
      | len >= bs = do
        k0 <- getWord32le
        let !k1 = k0 * m
            !k2 = k1 `xor` do k1 `shiftR` 24
            !k3 = k2 * m
            !h1 = h0 * m
            !h2 = h1 `xor` k3
        go (len - bs) h2
      | otherwise = do
        lbs <- getRemainingLazyByteString
        let !k0 = runGet getWord32le $ lbs <> "\0\0\0\0"
            !h1 = h0 `xor` k0
            !h2 = if len == 0 then h1 else h1 * m
            !h3 = h2 `xor` do h2 `shiftR` 13
            !h4 = h3 * m
            !h5 = h4 `xor` do h4 `shiftR` 15
        pure h5
    !bs = 4
    !m = 0x5bd1e995

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
