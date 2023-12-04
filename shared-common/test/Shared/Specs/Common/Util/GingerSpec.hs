module Shared.Specs.Common.Util.GingerSpec where

import qualified Data.HashMap.Strict as HM
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Common.Util.Ginger

gingerSpec =
  describe "Ginger" $ do
    it "success" $ do
      -- GIVEN:
      let template = "Hello {{name}}"
      let variables = HM.fromList [("name", "Joe")]
      let expected = Right "Hello Joe"
      --   let expected = ""
      -- WHEN:
      let result = renderEither template variables
      -- THEN:
      result `shouldBe` expected
