module CurseTest where

import Data.Map.Strict qualified as Map
import HellSmack.Curse.API
import HellSmack.Util
import Http
import Path.IO
import Test.Tasty
import Test.Tasty.HUnit

test_sanity :: IO TestTree
test_sanity =
  newTLSManager <&> \mgr ->
    testCaseSteps "Curse API sanity checks" \step -> do
      step "constants"
      game@Game {..} <- usingReaderT mgr $ getGame minecraftGameId
      game ^. #name @?= "Minecraft"
      game ^. #id @?= minecraftGameId

      let css = Map.fromList $ fmapToFst (^. #gameCategoryId) categorySections

      Just modCS <- pure $ css ^. at minecraftModsSection
      modCS ^. #name @?= "Mods"
      modCS ^. #packageType @?= minecraftModPackageType

      Just modpackCS <- pure $ css ^. at minecraftModpacksSection
      modpackCS ^. #name @?= "Modpacks"
      modpackCS ^. #packageType @?= minecraftModpackPackageType

      step "API calls"
      addon <- usingReaderT mgr $ getAddon ocId
      addon ^. #id @?= ocId
      void $ usingReaderT mgr $ getAddonFilesByAddonId ocId

      step "fingerprinting"
      Just addonFile <- pure $ addon ^? #latestFiles . _head
      fp <- withSystemTempFile "" \path _ -> do
        usingReaderT mgr $ downloadToFile (addonFile ^. #downloadUrl) path HideProgress
        fingerprintFile path
      addonFile ^. #packageFingerprint @=? fp
      fpms <- usingReaderT mgr $ getFingerprintMatches [fp]
      elemOf (#exactMatches . each . #file . #id) (addonFile ^. #id) fpms
        @? "search for fingerprint match failed"
  where
    ocId = AddonId 223008

-- TODO test search
