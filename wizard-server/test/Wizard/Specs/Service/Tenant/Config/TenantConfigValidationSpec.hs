module Wizard.Specs.Service.Tenant.Config.TenantConfigValidationSpec where

import qualified Data.Map.Strict as M
import Test.Hspec

import Shared.Common.Model.Error.Error
import Wizard.Service.Tenant.Config.ConfigValidation
import WizardLib.Common.Localization.Messages.Public

tenantConfigValidationSpec =
  describe "TenantConfigValidation" $
    it "isValidOrganizationId" $ do
      let validationError =
            Just $ ValidationError [] (M.singleton "organizationId" [_ERROR_VALIDATION__INVALID_ORG_ID_FORMAT])
      isValidOrganizationId "cz" `shouldBe` Nothing
      isValidOrganizationId "base.global" `shouldBe` Nothing
      isValidOrganizationId "base.global.e" `shouldBe` Nothing
      isValidOrganizationId "a" `shouldBe` Nothing
      isValidOrganizationId "a-b" `shouldBe` Nothing
      isValidOrganizationId "a_bc" `shouldBe` Nothing
      isValidOrganizationId ".cz" `shouldBe` Nothing
      isValidOrganizationId "cz." `shouldBe` Nothing
      isValidOrganizationId "base.global." `shouldBe` Nothing
      isValidOrganizationId ".base.global" `shouldBe` Nothing
      isValidOrganizationId "base.global-" `shouldBe` Nothing
      isValidOrganizationId "base:global" `shouldBe` validationError
      isValidOrganizationId "base$global" `shouldBe` validationError
