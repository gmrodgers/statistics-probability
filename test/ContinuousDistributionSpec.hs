module ContinuousDistributionSpec
  ( mainSpec,
  )
where

import ContinuousDistribution
import Debug.Trace
import Expressions
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
  let res = numericalCDF (Exp n) 0 128 150000
   in res < 1.0 + err && res > 1.0 - err

expSpec :: Spec
expSpec =
  let withinErrorOf answer err x = (&&) (x < (answer + err)) (x > (answer - err))
   in describe "exp" $ do
        describe "cdf" $ do
          it "integrates to 1 as x -> infinity" $
            property $ forAll genPos (prop_integratesToOneOverFullDomain 0.01)
          it "integrates to correct probability " $
            cdf (Exp 1) 0 1 `shouldSatisfy` withinErrorOf 0.01 0.63212
        describe "numerical cdf" $ do
          it "integrates to 1 as x -> infinity" $
            property $ forAll genPos (prop_integratesToOneOverFullDomainViaNumericalMethod 0.05)
          it "integrates to correct probability " $
            numericalCDF (Exp 1) 0 1 1000 `shouldSatisfy` withinErrorOf 0.05 0.63212
        describe "statistics" $ do
          it "has correct mean" $
            let prop_hardcodedMeanShouldEqualCalculatedMean n =
                  let equivalent = Custom (exponentialPDF n) 0 128
                      exponential = Exp n
                   in withinErrorOf (mean exponential) 0.01 (mean equivalent)
             in property $ forAll genPos prop_hardcodedMeanShouldEqualCalculatedMean
          xit "has correct variance" $
            let prop_hardcodedVarianceShouldEqualCalculatedMean n =
                  let equivalent = Custom (exponentialPDF n) 0 128
                      exponential = Exp n
                   in traceShow (variance exponential, variance equivalent) $ withinErrorOf (variance exponential) 0.01 (variance equivalent)
             in property $ forAll genPos prop_hardcodedVarianceShouldEqualCalculatedMean
