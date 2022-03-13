{-# LANGUAGE NoMonoLocalBinds #-}

module HellSmack.Test.VanillaTest where

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import HellSmack.Http
import HellSmack.Util.Download (forConcurrentlyNetwork)
import HellSmack.Util.Minecraft
import HellSmack.Util.Path
import HellSmack.Vanilla
import Test.Tasty
import Test.Tasty.HUnit

test_manifests :: IO TestTree
test_manifests = withSystemTempDir "" \dir -> do
  dirConf <- newDirConfig dir
  mgr <- newTLSManager
  let run = usingReaderT (dirConf, mgr)
  pure $ testCaseSteps "Vanilla manifest parsing" \step -> do
    step "all versions"
    avm <- run getAllVersionsManifest
    assetsTypesAndUrls <-
      M.fromListWith S.union <$> forConcurrentlyNetwork (avm ^.. #versions . each . #id) \version -> run do
        vm <- getVersionManifest version
        assets <- getAssetIndex vm
        at <- assetsType assets
        pure (at, one $ vm ^. #assetIndex . #url)
    iforOf_ (ifolded . indices (isn't #_ModernAssetsType)) assetsTypesAndUrls \at urls ->
      assertBool [i|not exactly one url for assets type ${show at :: String}: ${show urls :: String}|] $
        S.size urls == 1

-- TODO expand
