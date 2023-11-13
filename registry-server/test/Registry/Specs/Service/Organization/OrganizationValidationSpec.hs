module Registry.Specs.Service.Organization.OrganizationValidationSpec where

import Data.Maybe
import Test.Hspec

import Registry.Service.Organization.OrganizationValidation

organizationValidationSpec =
  describe "Organization Service" $
    it "validateOrganizationId" $ do
      isNothing (validateOrganizationId "cz") `shouldBe` True
      isNothing (validateOrganizationId "base.organization") `shouldBe` True
      isNothing (validateOrganizationId "base.organization.e") `shouldBe` True
      isJust (validateOrganizationId "a") `shouldBe` True
      isJust (validateOrganizationId "a_b") `shouldBe` True
      isJust (validateOrganizationId "a_bc") `shouldBe` True
      isJust (validateOrganizationId ".cz") `shouldBe` True
      isJust (validateOrganizationId "cz.") `shouldBe` True
      isJust (validateOrganizationId "base.organization.") `shouldBe` True
      isJust (validateOrganizationId ".base.organization") `shouldBe` True
      isJust (validateOrganizationId "base.organization-") `shouldBe` True
