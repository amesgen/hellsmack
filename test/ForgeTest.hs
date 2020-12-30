module ForgeTest where

import HellSmack.Forge
import Test.Tasty
import Test.Tasty.HUnit

test_pre113Check :: TestTree
test_pre113Check = testCase "Forge version pre 1.13 check" do
  isPre113 (ForgeVersion "1.16.4-35.1.28") @?= Right False
  isPre113 (ForgeVersion "1.15-29.0.1") @?= Right False
  isPre113 (ForgeVersion "1.12.1-14.22.1.2480") @?= Right True
  isPre113 (ForgeVersion "1.4.5-6.4.2.447") @?= Right True
  isPre113 (ForgeVersion "1.7.10-10.13.2.1352-1.7.10") @?= Right True
