module HellSmack.Util.Download
  ( -- * SHA1
    SHA1 (unSHA1),
    makeSHA1,
    checkSha1,

    -- * Downloads
    Manager,
    HasManagerIO,
    downloadToFile,
    downloadMaybe,
    downloadMaybeHash,
    downloadHash,
    downloadMaybeJson,
    downloadCachedJson,
    downloadJson,

    -- * Concurrent downloads
    forConcurrentlyNetwork,
    forConcurrentlyNetwork_,
  )
where

import Conduit
import Control.Monad.Trans.Except
import Crypto.Hash.SHA1 qualified as SHA1
import Data.Aeson
import Data.ByteString.Base16 qualified as B
import Data.List (lookup)
import Data.Text.Encoding.Base16 qualified as T
import HellSmack.Util.Exception
import HellSmack.Util.Has
import HellSmack.Util.Terminal
import Network.HTTP.Client
import Network.URI (escapeURIString)
import Path.IO
import UnliftIO.Async
import UnliftIO.IO.File

-- $setup
-- >>> import "hellsmack" Prelude

type HasManagerIO r m = (MonadIO m, MRHas r Manager m)

newtype SHA1 = SHA1 {unSHA1 :: Text}
  deriving stock (Show, Eq)
  deriving newtype (ToJSON)

instance FromJSON SHA1 where
  parseJSON = parseJSON >=> makeSHA1

-- $
-- >>> makeSHA1' = makeSHA1 @Maybe
-- >>> makeSHA1' "23e29655ca076534abc07bc0ad5f5c86c28f67a4"
-- Just (SHA1 {unSHA1 = "23e29655ca076534abc07bc0ad5f5c86c28f67a4"})
-- >>> makeSHA1' "23E29655CA076534ABC07BC0AD5F5C86C28F67A4"
-- Just (SHA1 {unSHA1 = "23e29655ca076534abc07bc0ad5f5c86c28f67a4"})
-- >>> makeSHA1' "23E29655CA076534ABC?07BC0AD5F5C86C28F67A"
-- Nothing
-- >>> makeSHA1' ""
-- Nothing
-- >>> makeSHA1' "23e29655ca076534abc07bc0ad5f5c86c28f67a45"
-- Nothing

makeSHA1 :: MonadFail m => Text -> m SHA1
makeSHA1 = \case
  h | T.isValidBase16 h && T.length h == 40 -> pure . SHA1 $ T.toLower h
  h -> fail [i|not a SHA1 hex string: $h|]

checkSha1 :: MonadIO m => Path Abs File -> Maybe SHA1 -> m (Either String ())
checkSha1 fp sha1 =
  doesFileExist fp >>= \case
    True -> case sha1 of
      Just sha1 -> do
        actualSha1 <- liftIO $ withBinaryFile (toFilePath fp) ReadMode \h -> runConduit do
          sourceHandleUnsafe h
            .| foldlC SHA1.update SHA1.init
            <&> SHA1 . B.encodeBase16 . SHA1.finalize
        pure $ unless (sha1 == actualSha1) $ Left [i|invalid hash for local file ${show fp}|]
      Nothing -> pure pass
    False -> pure $ Left [i|file ${show fp} does not exist|]

downloadToFile ::
  HasManagerIO r m =>
  -- | url
  Text ->
  -- | file path
  Path Abs File ->
  ProgressOption ->
  m ()
downloadToFile url path po =
  sieh >>= \mgr -> liftIO do
    -- some Curse urls contain unescaped ['s and ]'s
    req <- parseUrlThrow . escapeURIString (`notElem` ['[', ']']) . toString $ url
    withResponse req mgr \res -> do
      let contentLength = res & responseHeaders & lookup "content-length" >>= readMaybe . decodeUtf8
          withProgress f = case po of
            ShowProgress | Just fs <- contentLength -> withBytesProgress fs f
            _ -> f $ const pass
      withProgress \mod ->
        let download = repeatWhileMC (lift $ responseBody res) (not . B.null)
            updatePB = iterMC (mod . (+) . B.length)
         in runConduitRes $ download .| updatePB .| sinkFileCautious (toFilePath path)

downloadMaybe ::
  HasManagerIO r m =>
  -- | url
  Text ->
  -- | file path
  Path Abs File ->
  ProgressOption ->
  -- | validate file
  (Path Abs File -> m (Either String a)) ->
  m a
downloadMaybe url path po (($ path) -> validate) = do
  unlessM (doesFileExist path) download
  validate >>= \case
    Right a -> pure a
    Left _ -> do
      download
      validate >>= rethrow
  where
    download = do
      ensureDir $ parent path
      downloadToFile url path po

downloadMaybeHash ::
  HasManagerIO r m =>
  -- | url
  Text ->
  -- | file path
  Path Abs File ->
  -- | SHA1 hash, if available
  Maybe SHA1 ->
  ProgressOption ->
  -- | validate file
  (Path Abs File -> m (Either String a)) ->
  m a
downloadMaybeHash url path sha1 po validate =
  downloadMaybe url path po \fp -> runExceptT do
    ExceptT $ checkSha1 fp sha1
    ExceptT $ validate fp

downloadHash ::
  HasManagerIO r m =>
  -- | url
  Text ->
  -- | file path
  Path Abs File ->
  -- | SHA1 hash, if available
  Maybe SHA1 ->
  ProgressOption ->
  m ()
downloadHash url path sha1 po =
  downloadMaybeHash url path sha1 po $ const $ pure pass

downloadMaybeJson ::
  (HasManagerIO r m, FromJSON a) =>
  -- | url
  Text ->
  -- | file path
  Path Abs File ->
  -- | SHA1 hash, if available
  Maybe SHA1 ->
  -- | validate JSON
  (a -> Either String b) ->
  m b
downloadMaybeJson url path sha1 validate =
  downloadMaybeHash url path sha1 HideProgress \fp -> runExceptT do
    a <- either throwE pure . eitherDecode' =<< liftIO do readFileLBS (toFilePath fp)
    hoistEither $ validate a

downloadCachedJson ::
  (HasManagerIO r m, FromJSON a) =>
  -- | url
  Text ->
  -- | file path
  Path Abs File ->
  -- | SHA1 hash, if available
  Maybe SHA1 ->
  m a
downloadCachedJson url path sha1 =
  downloadMaybeJson url path sha1 pure

downloadJson :: (HasManagerIO r m, FromJSON a) => Text -> m a
downloadJson url =
  sieh >>= \mgr -> liftIO do
    req <- parseUrlThrow . toString $ url
    rethrow . eitherDecode' . responseBody =<< httpLbs req mgr

-- use some heuristic?
downloadParallelism :: Int
downloadParallelism = 40

forConcurrentlyNetwork :: (MonadUnliftIO m, Traversable t) => t a -> (a -> m b) -> m (t b)
forConcurrentlyNetwork = pooledForConcurrentlyN downloadParallelism

forConcurrentlyNetwork_ :: (MonadUnliftIO m, Foldable t) => t a -> (a -> m b) -> m ()
forConcurrentlyNetwork_ = pooledForConcurrentlyN_ downloadParallelism
