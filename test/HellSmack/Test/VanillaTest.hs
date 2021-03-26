{-# LANGUAGE NoMonoLocalBinds #-}

module HellSmack.Test.VanillaTest where

import HellSmack.Http
import HellSmack.Util.Download (forConcurrentlyNetwork_)
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
    forConcurrentlyNetwork_ (avm ^.. #versions . each . #id) \version -> run do
      vm <- getVersionManifest version
      void $ getAssetIndex vm

-- TODO expand
