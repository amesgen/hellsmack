{-# OPTIONS_GHC -Wno-orphans #-}

module HellSmack.Util.Orphans () where

import Data.Text.Lazy.Builder qualified as TLB
import Path

instance Display (Path b t) where
  displayBuilder = TLB.fromString . toFilePath
