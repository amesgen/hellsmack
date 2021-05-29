module HellSmack.Util.IO
  ( removeFileForce,
  )
where

import Path.IO
import System.IO.Error (isDoesNotExistError)
import UnliftIO.Exception

removeFileForce :: MonadIO m => Path b File -> m ()
removeFileForce = liftIO . handleJust (guarded isDoesNotExistError) (pure pass) . removeFile
