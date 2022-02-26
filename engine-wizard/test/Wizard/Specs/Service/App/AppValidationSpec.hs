module Wizard.Specs.Service.App.AppValidationSpec where

import qualified Data.Map.Strict as M
import Test.Hspec

import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Service.App.AppValidation

appValidationSpec =
  describe "AppValidationSpec" $ do
    it "isValidAppIdFormat" $ do
      let validationError word =
            Just $ ValidationError [] (M.singleton "appId" [_ERROR_VALIDATION__FORBIDDEN_CHARACTERS word])
      isValidAppIdFormat "a" `shouldBe` Nothing
      isValidAppIdFormat "ab" `shouldBe` Nothing
      isValidAppIdFormat "abc" `shouldBe` Nothing
      isValidAppIdFormat "abc1" `shouldBe` Nothing
      isValidAppIdFormat "1abc" `shouldBe` Nothing
      isValidAppIdFormat "a-b" `shouldBe` Nothing
      isValidAppIdFormat "ab-ab" `shouldBe` Nothing
      isValidAppIdFormat "ab1-a2b" `shouldBe` Nothing
      isValidAppIdFormat "" `shouldBe` validationError ""
      isValidAppIdFormat "-" `shouldBe` validationError "-"
      isValidAppIdFormat "ab-" `shouldBe` validationError "ab-"
      isValidAppIdFormat "-ab" `shouldBe` validationError "-ab"
      isValidAppIdFormat "a*b" `shouldBe` validationError "a*b"
      isValidAppIdFormat "č" `shouldBe` validationError "č"
