{-# LANGUAGE NoMonoLocalBinds #-}

module HellSmack.Test.CurseTest where

import HellSmack.Curse.API
import HellSmack.Http
import HellSmack.Util
import Path.IO
import Test.Tasty
import Test.Tasty.HUnit

test_sanity :: IO TestTree
test_sanity = do
  mgr <- newTLSManager
  let run = usingReaderT mgr
  pure $ testCaseSteps "Curse API sanity checks" \step -> do
    step "constants"
    game@Game {..} <- run $ getGame minecraftGameId
    game ^. #name @?= "Minecraft"
    game ^. #id @?= minecraftGameId

    let css = M.fromList $ fmapToFst (^. #gameCategoryId) categorySections

    Just modCS <- pure $ css ^. at minecraftModsSection
    modCS ^. #name @?= "Mods"
    modCS ^. #packageType @?= minecraftModPackageType

    Just modpackCS <- pure $ css ^. at minecraftModpacksSection
    modpackCS ^. #name @?= "Modpacks"
    modpackCS ^. #packageType @?= minecraftModpackPackageType

    cs <- run $ M.fromList . fmapToFst (^. #id) <$> getCategories

    Just fabricModsC <- pure $ cs ^. at fabricModsCategory
    fabricModsC ^. #id @?= fabricModsCategory
    fabricModsC ^. #name @?= "Fabric"
    fabricModsC ^. #slug @?= "fabric"

    step "API calls"
    addon <- run $ getAddon ocId
    addon ^. #id @?= ocId
    void $ run $ getAddonFilesByAddonId ocId

    step "fingerprinting"
    Just addonFile <- pure $ addon ^? #latestFiles . _head
    fp <- withSystemTempFile "" \path _ -> do
      run $ downloadToFile (addonFile ^. #downloadUrl) path HideProgress
      fingerprintFile path
    addonFile ^. #packageFingerprint @=? fp
    fpms <- run $ getFingerprintMatches [fp]
    elemOf (#exactMatches . each . #file . #id) (addonFile ^. #id) fpms
      @? "search for fingerprint match failed"
  where
    ocId = AddonId 223008

-- TODO test search
