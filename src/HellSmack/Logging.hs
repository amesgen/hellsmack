module HellSmack.Logging
  ( -- * Logging
    LogLevel (..),
    Logger (..),
    simpleLogger,

    -- ** logging helpers
    logLevel,
    logTrace,
    logDebug,
    logInfo,
    logWarn,
    logError,
  )
where

import Colourista.Pure qualified as C
import HellSmack.Util.Has

data LogLevel = LevelTrace | LevelDebug | LevelInfo | LevelWarn | LevelError
  deriving stock (Show, Ord, Eq, Generic, Bounded, Enum)

newtype Logger = Logger (LogLevel -> Text -> IO ())
  deriving stock (Generic)

logLevel :: (MonadIO m, MRHas r Logger m) => LogLevel -> Text -> m ()
logLevel lvl msg = sieh @Logger >>= \(Logger l) -> liftIO $ l lvl msg

logTrace :: (MonadIO m, MRHas r Logger m) => Text -> m ()
logTrace = logLevel LevelTrace

logDebug :: (MonadIO m, MRHas r Logger m) => Text -> m ()
logDebug = logLevel LevelDebug

logInfo :: (MonadIO m, MRHas r Logger m) => Text -> m ()
logInfo = logLevel LevelInfo

logWarn :: (MonadIO m, MRHas r Logger m) => Text -> m ()
logWarn = logLevel LevelWarn

logError :: (MonadIO m, MRHas r Logger m) => Text -> m ()
logError = logLevel LevelError

simpleLogger ::
  -- | minimum level
  LogLevel ->
  Logger
simpleLogger minLvl = Logger \lvl msg -> do
  let (cs, lvlTxt :: Text) = case lvl of
        LevelTrace -> ([], "[TRACE]")
        LevelDebug -> ([C.blue], "[DEBUG]")
        LevelInfo -> ([C.green], "[INFO ]")
        LevelWarn -> ([C.yellow], "[WARN ]")
        LevelError -> ([C.red], "[ERROR]")
  when (minLvl <= lvl) $ putTextLn [i|${C.formatWith cs lvlTxt} $msg|]
