module HellSmack.Util.Path
  ( makeSomeAbsolute,
    module Reexport,
  )
where

import Path as Reexport hiding ((<.>))
import Path.IO as Reexport

makeSomeAbsolute ::
  (MonadIO m, AnyPath (Path Rel t), AbsPath (Path Rel t) ~ Path Abs t) =>
  SomeBase t ->
  m (Path Abs t)
makeSomeAbsolute = \case
  Abs p -> pure p
  Rel p -> makeAbsolute p
