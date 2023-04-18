module Wizard.Specs.Service.Config.AppConfigValidationSpec where

import qualified Data.Map.Strict as M
import Test.Hspec

import Shared.Common.Model.Error.Error
import Wizard.Service.Config.App.AppConfigValidation
import WizardLib.Common.Localization.Messages.Public

appConfigValidationSpec =
  describe "AppConfigValidation" $
    it "isValidOrganizationId" $ do
      let validationError =
            Just $ ValidationError [] (M.singleton "organizationId" [_ERROR_VALIDATION__INVALID_ORG_ID_FORMAT])
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
