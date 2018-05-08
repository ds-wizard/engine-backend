module Specs.Service.Organization.OrganizationServiceSpec where

import Data.Maybe
import Test.Hspec

import Service.Organization.OrganizationService

organizationServiceSpec =
  describe "Organization Service" $
  it "isValidOrganizationId" $ do
    isNothing (isValidOrganizationId "cz") `shouldBe` True
    isNothing (isValidOrganizationId "base.elixir") `shouldBe` True
    isNothing (isValidOrganizationId "base.elixir.e") `shouldBe` True
    isJust (isValidOrganizationId "a") `shouldBe` True
    isJust (isValidOrganizationId "a-b") `shouldBe` True
    isJust (isValidOrganizationId "a_bc") `shouldBe` True
    isJust (isValidOrganizationId ".cz") `shouldBe` True
    isJust (isValidOrganizationId "cz.") `shouldBe` True
    isJust (isValidOrganizationId "base.elixir.") `shouldBe` True
    isJust (isValidOrganizationId ".base.elixir") `shouldBe` True
    isJust (isValidOrganizationId "base.elixir-") `shouldBe` True
