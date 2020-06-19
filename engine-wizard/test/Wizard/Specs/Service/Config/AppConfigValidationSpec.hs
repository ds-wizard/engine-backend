module Wizard.Specs.Service.Config.AppConfigValidationSpec where

import Test.Hspec

import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Service.Config.AppConfigValidation

appConfigValidationSpec =
  describe "AppConfigValidation" $ do
    let validationError = Just $ ValidationError [] [("organizationId", _ERROR_VALIDATION__INVALID_ORG_ID_FORMAT)]
    it "isValidOrganizationId" $ do
      isValidOrganizationId "cz" `shouldBe` Nothing
      isValidOrganizationId "base.global" `shouldBe` Nothing
      isValidOrganizationId "base.global.e" `shouldBe` Nothing
      isValidOrganizationId "a" `shouldBe` validationError
      isValidOrganizationId "a-b" `shouldBe` validationError
      isValidOrganizationId "a_bc" `shouldBe` validationError
      isValidOrganizationId ".cz" `shouldBe` validationError
      isValidOrganizationId "cz." `shouldBe` validationError
      isValidOrganizationId "base.global." `shouldBe` validationError
      isValidOrganizationId ".base.global" `shouldBe` validationError
      isValidOrganizationId "base.global-" `shouldBe` validationError
