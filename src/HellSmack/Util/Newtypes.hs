module HellSmack.Util.Newtypes
  ( ShowWithoutQuotes (..),
  )
where

import Text.Show qualified

newtype ShowWithoutQuotes a = ShowWithoutQuotes a

instance ToString a => Show (ShowWithoutQuotes a) where
  show = toString @a . coerce
