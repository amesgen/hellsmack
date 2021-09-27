{-# LANGUAGE CPP #-}

module HellSmack.Http (newTLSManager, Manager) where

import Network.HTTP.Client
#if USE_HASKELL_TLS
import Network.HTTP.Client.TLS
#else
import Network.HTTP.Client.OpenSSL
#endif

newTLSManager :: MonadIO m => m Manager
#if USE_HASKELL_TLS
newTLSManager = newTlsManager
#else
newTLSManager = liftIO $ withOpenSSL newOpenSSLManager
#endif
