{-# LANGUAGE CPP #-}

module HellSmack.Http (newTLSManager, Manager) where

import Network.HTTP.Client
#if USE_OPENSSL
import Network.HTTP.Client.OpenSSL
#else
import Network.HTTP.Client.TLS
#endif

newTLSManager :: MonadIO m => m Manager
#if USE_OPENSSL
newTLSManager = liftIO $ withOpenSSL newOpenSSLManager
#else
newTLSManager = newTlsManager
#endif
