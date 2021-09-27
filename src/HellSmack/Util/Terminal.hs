module HellSmack.Util.Terminal
  ( -- * Terminal utilities

    -- ** Print bytes
    showBytes,
    showBytes',

    -- ** TTY
    clearLastLines,

    -- ** Prompts
    promptVia,
    promptBool,

    -- ** Tables
    formattedVia,
    selectViaTable,

    -- ** Progress bars
    ProgressOption (..),
    withGenericProgress,
    withBytesProgress,
    stepWise,
  )
where

import Colourista.Pure qualified as C
import Data.Text qualified as T
import Data.Time
import Data.Time.Clock.System
import Data.Time.Clock.TAI
import System.Console.ANSI qualified as AT
import Text.Layout.Table qualified as TL
import Text.Layout.Table.Cell.Formatted qualified as TL
import Text.Printf (printf)
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Exception
import UnliftIO.IO (hIsTerminalDevice)

showBytes :: Integral a => a -> a -> Text
showBytes (fromIntegral -> ref :: Double) (fromIntegral -> a :: Double) =
  prefixes
    >>= do \(p, exp) -> guard (ref < base ^ (exp + expd)) $> pr (a / base ^ exp) <> " " <> p <> "B"
    & (^?! _head)
  where
    pr = toText @String . printf "%.2f"
    prefixes = ["", "K", "M", "G", "T", "P", "E", "Z", "Y"] `zip` [0, expd ..]
    expd = 3 :: Int
    base = 10

showBytes' :: Integral a => a -> Text
showBytes' = join showBytes

clearLastLines :: MonadIO m => Int -> m ()
clearLastLines n = liftIO do
  AT.setCursorColumn 0
  AT.cursorUp n
  AT.clearFromCursorToScreenEnd

promptVia :: MonadIO m => Text -> (Text -> m (Maybe a)) -> m a
promptVia txt validate =
  fix \go ->
    putText txt *> hFlush stdout *> (getLine >>= validate) <* clearLastLines 1 >>= \case
      Just a -> pure a
      Nothing -> go

promptBool :: MonadIO m => Text -> m Bool
promptBool txt =
  promptVia [i|$txt (y/n): |] $
    T.toLower >>> \case
      "y" -> pure $ Just True
      "n" -> pure $ Just False
      _ -> pure Nothing

selectViaTable ::
  MonadIO m =>
  NonEmpty a ->
  [String] ->
  (a -> [TL.Formatted String]) ->
  m a
selectViaTable items' (("#" :) -> titles) toRow = do
  let items = toList items'
      table =
        TL.tableLines
          do titles $> TL.def
          tableStyle
          do TL.titlesH titles
          do TL.rowG <$> zipWith (:) (TL.plain . show <$> [0 :: Int ..]) (toRow <$> items)
  putStrLn $ intercalate "\n" table
  let yieldItem a = clearLastLines (length table) $> Just a
  promptVia "choose (default: 0): " \case
    "" -> yieldItem $ head items'
    (toString >>> readMaybe >=> (items !!?) -> Just a) -> yieldItem a
    _ -> pure Nothing

formattedVia :: String -> String -> TL.Formatted String
formattedVia c a = TL.formatted c a C.reset

tableStyle :: TL.TableStyle
tableStyle = TL.unicodeS

data ProgressOption = ShowProgress | HideProgress
  deriving stock (Show, Ord, Eq, Generic, Enum, Bounded)

withProgress ::
  (MonadUnliftIO m, MonadIO m2, Integral n) =>
  (n -> n -> Text) ->
  n ->
  (((n -> n) -> m2 ()) -> m a) ->
  m a
withProgress showN' maxCount cb =
  hIsTerminalDevice stdout >>= \case
    True -> do
      barLength <-
        liftIO AT.getTerminalSize <&> \case
          Just (_, width) -> (width - T.length (barTxt 0 "" maxCount) - 1) `min` 80
          Nothing -> 80
      countRef <- newIORef 0
      start <- currentTime
      let withProgressBar = withAsyncWithUnmask \unmask -> infinitely do
            count <- readIORef countRef
            diff <- diffAbsoluteTime <$> currentTime <*> pure start
            let p = round @Double $ fromIntegral count / fromIntegral maxCount * fromIntegral barLength
                fW c = C.formatWith [c]
                bar = fW C.green (T.replicate p "#") <> fW C.blue (T.replicate (barLength - p) "-")
            putTextLn $ barTxt diff bar count
            unmask (threadDelay printTickMicros) `finally` clearLastLines 1
      withProgressBar \_ -> cb $ atomicModifyIORef'_ countRef
    False -> cb $ const pass
  where
    barTxt diff bar count = [i|[$diffTxt] $bar ${} / ${}|] (showN count) (showN maxCount)
      where
        showN = showN' maxCount
        diffTxt = formatTime defaultTimeLocale "%02H:%02M:%02S" diff
    currentTime = liftIO $ systemToTAITime <$> getSystemTime
    printTickMicros = 100 * 1000

withGenericProgress ::
  (MonadUnliftIO m, MonadIO m2, Integral n, Show n) =>
  n ->
  (((n -> n) -> m2 ()) -> m a) ->
  m a
withGenericProgress = withProgress $ const show

withBytesProgress ::
  (MonadUnliftIO m, MonadIO m2, Integral n) =>
  n ->
  (((n -> n) -> m2 ()) -> m a) ->
  m a
withBytesProgress = withProgress showBytes

stepWise ::
  (MonadIO m2, Integral n) =>
  (forall m2. MonadIO m2 => (((n -> n) -> m2 ()) -> m a) -> m a) ->
  (((forall b. m2 b -> m2 b) -> m a) -> m a)
stepWise wp cb = wp \mod -> cb \mb -> mb <* mod (+ 1)
