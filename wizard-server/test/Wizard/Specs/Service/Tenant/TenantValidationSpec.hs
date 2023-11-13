module Wizard.Specs.Service.Tenant.TenantValidationSpec where

import qualified Data.Map.Strict as M
import Test.Hspec

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Service.Tenant.TenantValidation

tenantValidationSpec =
  describe "TenantValidationSpec" $ do
    it "isValidTenantIdFormat" $ do
      let validationError word =
            Just $ ValidationError [] (M.singleton "tenantId" [_ERROR_VALIDATION__FORBIDDEN_CHARACTERS word])
      isValidTenantIdFormat "a" `shouldBe` Nothing
      isValidTenantIdFormat "ab" `shouldBe` Nothing
      isValidTenantIdFormat "abc" `shouldBe` Nothing
      isValidTenantIdFormat "abc1" `shouldBe` Nothing
      isValidTenantIdFormat "1abc" `shouldBe` Nothing
      isValidTenantIdFormat "a-b" `shouldBe` Nothing
      isValidTenantIdFormat "ab-ab" `shouldBe` Nothing
      isValidTenantIdFormat "ab1-a2b" `shouldBe` Nothing
      isValidTenantIdFormat "" `shouldBe` validationError ""
      isValidTenantIdFormat "-" `shouldBe` validationError "-"
      isValidTenantIdFormat "ab-" `shouldBe` validationError "ab-"
      isValidTenantIdFormat "-ab" `shouldBe` validationError "-ab"
      isValidTenantIdFormat "a*b" `shouldBe` validationError "a*b"
      isValidTenantIdFormat "č" `shouldBe` validationError "č"
