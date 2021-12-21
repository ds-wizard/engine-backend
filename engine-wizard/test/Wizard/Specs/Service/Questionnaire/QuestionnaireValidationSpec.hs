module Wizard.Specs.Service.Questionnaire.QuestionnaireValidationSpec where

import qualified Data.Map.Strict as M
import Test.Hspec

import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Service.Questionnaire.QuestionnaireValidation

questionnaireValidationSpec =
  describe "QuestionnaireValidation" $ do
    it "isValidProjectTag" $ do
      let validationError word =
            Just $ ValidationError [] (M.singleton "tags" [_ERROR_VALIDATION__FORBIDDEN_CHARACTERS word])
      isValidProjectTag "a" `shouldBe` Nothing
      isValidProjectTag "ab" `shouldBe` Nothing
      isValidProjectTag "aB" `shouldBe` Nothing
      isValidProjectTag "ab c" `shouldBe` Nothing
      isValidProjectTag "ab_c" `shouldBe` Nothing
      isValidProjectTag "ab-c" `shouldBe` Nothing
      isValidProjectTag "ab-,c" `shouldBe` validationError "ab-,c"
