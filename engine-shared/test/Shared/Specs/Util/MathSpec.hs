module Shared.Specs.Util.MathSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Util.Math

mathSpec =
  describe "Math" $
  describe "weightAverage" $ do
    it "[] returns 0" $ weightAverage [] `shouldBe` 0
    it "[(2,1),(3,3),(17,2)] returns Just [a, b]" $ weightAverage [(2, 1), (3, 3), (17, 2)] `shouldBe` 7.5
