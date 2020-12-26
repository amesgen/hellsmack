{-# LANGUAGE CPP #-}

module Http (withHttpManager, Manager) where

import Network.HTTP.Client hiding (withManager)
import UnliftIO (MonadUnliftIO (..))

#if USE_OPENSSL
import Network.HTTP.Client.OpenSSL
#else
import Network.HTTP.Client.TLS
#endif

withHttpManager :: MonadUnliftIO m => (m Manager -> m a) -> m a

#if USE_OPENSSL
withHttpManager cb = withRunInIO \toIO ->
  withOpenSSL $ toIO $ cb $ newOpenSSLManager
#else
withHttpManager cb = cb newTlsManager
#endif
