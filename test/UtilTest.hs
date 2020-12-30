module UtilTest where

import Data.Binary.Get
import Data.ByteString.Lazy qualified as BL
import HellSmack.Util
import Test.Tasty
import Test.Tasty.HUnit

test_mavenIdParsing :: TestTree
test_mavenIdParsing =
  testGroup "Maven ID parsing" $
    [ testCase "positive tests" do
        parseMavenId' "foo.bar:baz:1.1" @?= Right do
          mavenId "foo.bar" "baz" "1.1"
        parseMavenId' "baz.foo:bar:1.2-dev:test" @?= Right do
          mavenId "baz.foo" "bar" "1.2-dev" & #classifier ?~ "test"
        parseMavenId' "baz.foo:bar:1.2-dev@exe" @?= Right do
          mavenId "baz.foo" "bar" "1.2-dev" & #extension ?~ "exe"
        parseMavenId' "baz.foo:bar:1.3:test@exe" @?= Right do
          mavenId "baz.foo" "bar" "1.3" & #classifier ?~ "test" & #extension ?~ "exe",
      testCase "negative tests" do
        parseMavenId "wtf" @?= Nothing
        parseMavenId "wtf:wtf" @?= Nothing
        parseMavenId "wtf:wtf:wtf:wtf:wtf" @?= Nothing
        parseMavenId "wtf:wtf@exe" @?= Nothing,
      testCase "path rendering" do
        Just [relfile|foo/bar/baz/1.1/baz-1.1.jar|] @=? mavenIdPath do
          mavenId "foo.bar" "baz" "1.1"
        Just [relfile|baz/foo/bar/1.2-dev/bar-1.2-dev-test.jar|] @=? mavenIdPath do
          mavenId "baz.foo" "bar" "1.2-dev" & #classifier ?~ "test"
        Just [relfile|baz/foo/bar/1.2-dev/bar-1.2-dev.exe|] @=? mavenIdPath do
          mavenId "baz.foo" "bar" "1.2-dev" & #extension ?~ "exe"
        Just [relfile|baz/foo/bar/1.3/bar-1.3-test.exe|] @=? mavenIdPath do
          mavenId "baz.foo" "bar" "1.3" & #classifier ?~ "test" & #extension ?~ "exe"
    ]
  where
    parseMavenId' = parseMavenId @(Either String)
    mavenId groupId artifactId version =
      MavenId {classifier = Nothing, extension = Nothing, ..}

test_murmur :: TestTree
test_murmur = testCase "MurmurHash2 (32bit)" do
  mh "" 0 @?= 0
  mh "a" 1 @?= 626045324
  mh "bb" 2 @?= 1692487918
  mh "ccc" 3 @?= 1021219781
  mh "dddd" 4 @?= 4155289461
  mh "eeeee" 5 @?= 2583669252
  mh "ffffff" 6 @?= 2512034341
  mh "ggggggg" 7 @?= 3016870013
  mh "hhhhhhhh" 8 @?= 3696140734
  mh "randomshit" 42 @?= 2946655983
  mh "aseofansepfansepfnaspebfsapebfapsbeapsebufapsefbpas" 420 @?= 682730836
  where
    mh bs seed = runGet (murmurhash (fromIntegral $ BL.length bs) seed) bs
