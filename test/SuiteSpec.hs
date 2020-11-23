module SuiteSpec
  ( main,
  )
where

import ContinuousDistributionSpec (mainSpec)
import ExpressionsSpec (mainSpec)
import Test.Hspec (hspec, parallel)

main :: IO ()
main = (hspec . parallel) $ do
  ExpressionsSpec.mainSpec
  ContinuousDistributionSpec.mainSpec