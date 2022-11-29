module Wizard.Specs.Service.Branch.BranchValidationSpec where

import Data.Maybe
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Wizard.Service.Branch.BranchValidation

branchValidationSpec =
  describe "BranchValidation" $
    it "validateVersionFormat" $ do
      isNothing (isValidKmId "core") `shouldBe` True
      isNothing (isValidKmId "ab") `shouldBe` True
      isNothing (isValidKmId "core-nl") `shouldBe` True
      isNothing (isValidKmId "core-nl-amsterdam") `shouldBe` True
      isJust (isValidKmId "a") `shouldBe` True
      isJust (isValidKmId "core.nl") `shouldBe` True
      isJust (isValidKmId "a.b") `shouldBe` True
      isJust (isValidKmId "core_nl") `shouldBe` True
