module ContinuousDistributionSpec
  ( mainSpec,
  )
where

import ContinuousDistribution
import Test.Hspec
import Test.QuickCheck

mainSpec :: Spec
mainSpec = parallel $
  describe "continuous distributions" $ do
    expSpec

genPos :: Gen Float
genPos = abs `fmap` (arbitrary :: Gen Float) `suchThat` (> 0.1)

prop_integratesToOneOverFullDomain :: Float -> Float -> Bool
prop_integratesToOneOverFullDomain err n =
  let res = cdf (Exp n) 0 128
   in res < 1.0 + err && res > 1.0 - err

expSpec :: Spec
expSpec =
  let err = 0.01
   in describe "exp cdf" $ do
        it "integrates to 1 as x -> infinity" $
          quickCheck $ forAll genPos (prop_integratesToOneOverFullDomain err)
        it "integrates to correct probability " $
          abs (cdf (Exp 1) 0 1) - 0.63212 < err `shouldBe` True