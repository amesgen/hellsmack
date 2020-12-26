module HellSmack.Util.Download
  ( -- * Sha1
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
import Data.ByteString qualified as B
import Data.ByteString.Base16
import Data.List (lookup)
import HellSmack.Util.Exception
import HellSmack.Util.Has
import HellSmack.Util.Terminal
import Network.HTTP.Client
import Network.URI (escapeURIString)
import Path.IO
import UnliftIO.Async
import UnliftIO.IO.File

type HasManagerIO r m = (MonadIO m, MRHas r Manager m)

checkSha1 :: MonadIO m => Path Abs File -> (Text -> Bool) -> m (Either String ())
checkSha1 fp sha1Valid =
  doesFileExist fp >>= \case
    True -> do
      actualSha1 <- liftIO $ withBinaryFile (toFilePath fp) ReadMode \h -> runConduit do
        sourceHandleUnsafe h
          .| foldlC SHA1.update SHA1.init
          <&> encodeBase16 . SHA1.finalize
      pure . unless (sha1Valid actualSha1) $
        Left [i|invalid hash for local file #{fp}|]
    False -> pure $ Left [i|file #{fp} does not exist|]

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
  Maybe Text ->
  ProgressOption ->
  -- | validate file
  (Path Abs File -> m (Either String a)) ->
  m a
downloadMaybeHash url path sha1 po validate =
  downloadMaybe url path po \fp -> runExceptT do
    ExceptT $ checkSha1 fp \actualSha1 -> sha1 & all (== actualSha1)
    ExceptT $ validate fp

downloadHash ::
  HasManagerIO r m =>
  -- | url
  Text ->
  -- | file path
  Path Abs File ->
  -- | SHA1 hash, if available
  Maybe Text ->
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
  Maybe Text ->
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
  Maybe Text ->
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
