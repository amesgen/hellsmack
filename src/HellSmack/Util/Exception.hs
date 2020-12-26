module HellSmack.Util.Exception
  ( -- * Exceptions
    rethrow,
    reThrow,
  )
where

import Control.Monad.Catch.Pure
import UnliftIO.Exception

rethrow :: (MonadIO m, HasCallStack) => Either String a -> m a
rethrow = either throwString pure

reThrow :: MonadIO m => CatchT m a -> m a
reThrow = runCatchT >=> either throwIO pure
