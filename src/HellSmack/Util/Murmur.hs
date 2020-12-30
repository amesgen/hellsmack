module HellSmack.Util.Murmur
  ( murmurhash,
  )
where

import Data.Binary.Get
import Data.Bits (shiftR)

-- | MurmurHash2 (32bit)
murmurhash ::
  -- |  length
  Word32 ->
  -- |  seed
  Word32 ->
  Get Word32
murmurhash !len' !seed = go len' (seed `xor` len')
  where
    go !len !h0
      | len >= bs = do
        k0 <- getWord32le
        let !k1 = k0 * m
            !k2 = k1 `xor` do k1 `shiftR` 24
            !k3 = k2 * m
            !h1 = h0 * m
            !h2 = h1 `xor` k3
        go (len - bs) h2
      | otherwise = do
        lbs <- getRemainingLazyByteString
        let !k0 = runGet getWord32le $ lbs <> "\0\0\0\0"
            !h1 = h0 `xor` k0
            !h2 = if len == 0 then h1 else h1 * m
            !h3 = h2 `xor` do h2 `shiftR` 13
            !h4 = h3 * m
            !h5 = h4 `xor` do h4 `shiftR` 15
        pure h5
    !bs = 4
    !m = 0x5bd1e995
