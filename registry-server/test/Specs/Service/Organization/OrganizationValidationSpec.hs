module Specs.Service.Organization.OrganizationValidationSpec where

import Data.Maybe
import Test.Hspec

import Service.Organization.OrganizationValidation

organizationValidationSpec =
  describe "Organization Service" $
  it "validateOrganizationId" $ do
    isNothing (validateOrganizationId "cz") `shouldBe` True
    isNothing (validateOrganizationId "base.dsw") `shouldBe` True
    isNothing (validateOrganizationId "base.dsw.e") `shouldBe` True
    isJust (validateOrganizationId "a") `shouldBe` True
    isJust (validateOrganizationId "a-b") `shouldBe` True
    isJust (validateOrganizationId "a_bc") `shouldBe` True
    isJust (validateOrganizationId ".cz") `shouldBe` True
    isJust (validateOrganizationId "cz.") `shouldBe` True
    isJust (validateOrganizationId "base.dsw.") `shouldBe` True
    isJust (validateOrganizationId ".base.dsw") `shouldBe` True
    isJust (validateOrganizationId "base.dsw-") `shouldBe` True
