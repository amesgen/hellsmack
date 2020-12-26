module HellSmack.Util.Has
  ( MRHas,
    MRHasAll,
    sieh,
    siehs,
  )
where

import Data.Generics.Product.Typed

type MRHas r s m = (MonadReader r m, HasType s r)

type family MRHasAll r s m where
  MRHasAll r '[] m = MonadReader r m
  MRHasAll r (s ': ss) m = (HasType s r, MRHasAll r ss m)

sieh :: forall s m r. MRHas r s m => m s
sieh = view typed

siehs :: forall s a m r. MRHas r s m => Getter s a -> m a
siehs g = view $ typed . g
