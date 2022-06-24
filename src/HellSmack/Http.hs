module HellSmack.Http (newTLSManager) where

import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.Rustls (rustlsManagerSettings)
import Rustls qualified
import UnliftIO.Exception

newTLSManager :: MonadIO m => m HTTP.Manager
newTLSManager = liftIO do
  roots <-
    fmap (Rustls.ClientRootsInMemory . pure . Rustls.PEMCertificatesStrict) $
      defaultCertFile `onException` envCertFile
  clientConfig <- Rustls.buildClientConfig $ Rustls.defaultClientConfigBuilder roots
  HTTP.newManager $ rustlsManagerSettings clientConfig
  where
    defaultCertFile = readFileBS "/etc/ssl/certs/ca-certificates.crt"
    envCertFile =
      lookupEnv envKey >>= \case
        Just file | not (null file) -> readFileBS file
        _ -> throwString [i|default SSL certs not found, please set $envKey|]
      where
        envKey :: String
        envKey = "SSL_CERT_FILE"
