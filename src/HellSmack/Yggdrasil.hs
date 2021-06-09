module HellSmack.Yggdrasil
  ( AccessToken (..),
    MCAuth (..),
    bogusMCAuth,
    loadMCAuth,
    saveMCAuth,
    invalidateMCAuth,
  )
where

import HellSmack.Logging
import HellSmack.Util
import HellSmack.Yggdrasil.API
import UnliftIO.Exception

data MCAuth = MCAuth
  { username :: Text,
    uuid :: Text,
    accessToken :: AccessToken
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

bogusMCAuth :: MCAuth
bogusMCAuth = MCAuth {username = "foobar", uuid = "foobar", accessToken = AccessToken "foobar"}

authIso :: Iso' AuthResponse MCAuth
authIso =
  iso
    do \AuthResponse {..} -> MCAuth {username = name, ..}
    do \MCAuth {..} -> AuthResponse {name = username, ..}

loadAuthResponse :: MonadIO m => Path Abs File -> m AuthResponse
loadAuthResponse authPath = liftIO do
  unlessM (doesFileExist authPath) $ throwString "no auth file found - try to log in"
  readFileLBS (toFilePath authPath) >>= decodeJSON <&> (^. from authIso)

saveAuthResponse :: MonadIO m => Path Abs File -> AuthResponse -> m ()
saveAuthResponse authPath ar =
  writeFileLBS (toFilePath authPath) (encodeJSON $ ar ^. authIso)

loadMCAuth :: (MonadUnliftIO m, MRHasAll r [Manager, Logger] m) => Path Abs File -> m MCAuth
loadMCAuth authPath = do
  ar <- loadAuthResponse authPath
  ar <-
    validate (ar ^. #accessToken) >>= \case
      AccessTokenValid -> pure ar
      AccessTokenInvalid ->
        try (refresh (ar ^. #accessToken)) >>= \case
          Right ar -> saveAuthResponse authPath ar $> ar
          Left (e :: YggdrasilException) -> removeFile authPath *> throwIO e
  logInfo "authentication successful"
  pure $ ar ^. authIso

saveMCAuth ::
  (MonadIO m, MRHasAll r [Manager, Logger] m) =>
  Path Abs File ->
  -- | email
  Text ->
  -- | password
  Text ->
  m ()
saveMCAuth authPath email password = do
  ar <- authenticate email password
  saveAuthResponse authPath ar
  logInfo "authentication successful"

invalidateMCAuth :: (MonadUnliftIO m, MRHasAll r [Manager, Logger] m) => Path Abs File -> m ()
invalidateMCAuth authPath =
  whenM (doesFileExist authPath) do
    ar <- loadAuthResponse authPath
    invalidate (ar ^. #accessToken) `catchAny` do logDebug . [i|invalidation failed: $show|]
    removeFile authPath
