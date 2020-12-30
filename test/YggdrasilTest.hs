module YggdrasilTest where

import HellSmack.Yggdrasil.API
import Http
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO.Exception

test_yggdrasil :: IO TestTree
test_yggdrasil =
  newTLSManager <&> \mgr ->
    testGroup "Yggdrasil API" $
      [ testCase "authentication failure" do
          res <- usingReaderT mgr $ try $ authenticate "bo" "gus"
          isLeft (res :: Either YggdrasilException AuthResponse) @? "bogus auth did not fail"
      ]
