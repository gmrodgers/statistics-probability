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

prop_integratesToOneOverFullDomainViaNumericalMethod :: Float -> Float -> Bool
prop_integratesToOneOverFullDomainViaNumericalMethod err n =
  let res = numericalCDF (Exp n) 0 128 100
   in res < 1.0 + err && res > 1.0 - err

expSpec :: Spec
expSpec =
  let err = 0.01
      withinErrorOf answer x = (&&) (x < (answer + err)) (x > (answer - err))
   in describe "exp" $ do
        describe "cdf" $ do
          it "integrates to 1 as x -> infinity" $
            quickCheck $ forAll genPos (prop_integratesToOneOverFullDomain err)
          it "integrates to correct probability " $
            cdf (Exp 1) 0 1 `shouldSatisfy` withinErrorOf 0.63212
        describe "numerical cdf" $ do
          it "integrates to 1 as x -> infinity" $
            quickCheck $ forAll genPos (prop_integratesToOneOverFullDomainViaNumericalMethod err)
          it "integrates to correct probability " $
            numericalCDF (Exp 1) 0 1 1000 `shouldSatisfy` withinErrorOf 0.63212

        xdescribe "statistics" $ do
          it "has correct mean" $
            let equivalent = Custom (exponentialPDF 1.0) 0 128
                exponential = Exp 1.0
             in mean equivalent `shouldBe` mean exponential
