{-# LANGUAGE NoMonoLocalBinds #-}

module HellSmack.Test.VanillaTest where

import HellSmack.Http
import HellSmack.Util.Download (forConcurrentlyNetwork)
import HellSmack.Util.Minecraft
import HellSmack.Vanilla
import Path.IO
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
        pure (at, S.singleton $ vm ^. #assetIndex . #url)
    iforOf_ (ifolded . indices (isn't #_ModernAssetsType)) assetsTypesAndUrls \at urls ->
      assertBool [i|not exactly one url for assets type ${show at}: ${show urls}|] $
        S.size urls == 1

-- TODO expand
