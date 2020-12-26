module HellSmack.Util.Path
  ( makeSomeAbsolute,
  )
where

import Path.IO

makeSomeAbsolute ::
  (MonadIO m, AnyPath (Path Rel t), AbsPath (Path Rel t) ~ Path Abs t) =>
  SomeBase t ->
  m (Path Abs t)
makeSomeAbsolute = \case
  Abs p -> pure p
  Rel p -> makeAbsolute p
