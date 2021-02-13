module HellSmack.Yggdrasil.API
  ( -- * Types
    AuthResponse (..),
    AccessToken (..),
    AccessTokenValidity (..),

    -- ** Exception
    YggdrasilException (..),

    -- * Yggdrasil API
    authenticate,
    refresh,
    validate,
    signout,
    invalidate,
  )
where

import Data.Aeson
import HellSmack.Util
import HellSmack.Util.Meta qualified as Meta
import Network.HTTP.Client
import Network.HTTP.Types.Status (Status (..))
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception

data YggdrasilException = YggdrasilException
  { error :: Text,
    errorMessage :: Text,
    cause :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (Exception, FromJSON)

newtype AccessToken = AccessToken {unAccessToken :: Text}
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

data AuthResponse = AuthResponse
  { name :: Text,
    uuid :: Text,
    accessToken :: AccessToken
  }
  deriving stock (Show, Generic)

instance FromJSON AuthResponse where
  parseJSON = withObject "AuthResponse" \o -> do
    accessToken <- AccessToken <$> o .: "accessToken"
    profile <- o .: "selectedProfile"
    (name, uuid) <-
      profile & withObject "SelectedProfile" \o ->
        (,) <$> o .: "name" <*> o .: "id"
    pure AuthResponse {..}

baseUrl :: Text
baseUrl = "https://authserver.mojang.com"

postJSON :: (MonadIO m, MRHas r Manager m, ToJSON a) => Text -> a -> m LByteString
postJSON url a = do
  req <-
    liftIO $
      parseRequest [i|POST $baseUrl/$url|] <&> \req ->
        req
          { requestHeaders = ("Content-Type", "application/json") : requestHeaders req,
            requestBody = RequestBodyLBS . encode $ a
          }
  mgr <- sieh @Manager
  res <- liftIO $ httpLbs req mgr
  let body = responseBody res
  case responseStatus res of
    Status sc _ | 200 <= sc && sc < 300 -> pure body
    _ -> body & eitherDecode' @YggdrasilException & either throwString throwIO

postJSON' :: (MonadIO m, MRHas r Manager m, ToJSON a) => Text -> a -> m AuthResponse
postJSON' url a = postJSON url a <&> eitherDecode' >>= rethrow

clientToken :: Text
clientToken = Meta.name

authenticate ::
  (MonadIO m, MRHas r Manager m) =>
  -- | username
  Text ->
  -- | password
  Text ->
  m AuthResponse
authenticate username password =
  postJSON' "authenticate" $
    object
      [ ("agent", object [("name", String "Minecraft"), ("version", Number 1)]),
        ("username", String username),
        ("password", String password),
        ("clientToken", String clientToken),
        ("requestUser", Bool False)
      ]

refresh :: (MonadIO m, MRHas r Manager m) => AccessToken -> m AuthResponse
refresh (AccessToken at) =
  postJSON' "refresh" $
    object
      [ ("accessToken", String at),
        ("clientToken", String clientToken),
        ("requestUser", Bool False)
      ]

data AccessTokenValidity = AccessTokenValid | AccessTokenInvalid
  deriving stock (Show, Eq, Generic, Enum, Bounded)

validate :: (MonadUnliftIO m, MRHas r Manager m) => AccessToken -> m AccessTokenValidity
validate (AccessToken at) =
  ( postJSON "validate" (object [("accessToken", String at), ("clientToken", String clientToken)])
      $> AccessTokenValid
  )
    `catch` \e@YggdrasilException {..} ->
      if error == "ForbiddenOperationException" then pure AccessTokenInvalid else throwIO e

signout ::
  (MonadIO m, MRHas r Manager m) =>
  -- | username
  Text ->
  -- | password
  Text ->
  m ()
signout username password =
  void . postJSON "signout" $
    object [("username", String username), ("password", String password)]

invalidate :: (MonadIO m, MRHas r Manager m) => AccessToken -> m ()
invalidate (AccessToken at) =
  void . postJSON "invalidate" $
    object [("accessToken", String at), ("clientToken", String clientToken)]
