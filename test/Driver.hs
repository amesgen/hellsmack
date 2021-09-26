module Main (main) where

import HellSmack.Test.CurseTest
import HellSmack.Test.VanillaTest
import HellSmack.Test.YggdrasilTest
import Test.Tasty

main :: IO ()
main =
  defaultMain =<< do
    testGroup "Tests"
      <$> sequence
        [ test_sanity,
          test_manifests,
          test_yggdrasil
        ]
