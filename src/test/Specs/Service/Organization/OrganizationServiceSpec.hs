module Specs.Service.Organization.OrganizationServiceSpec where

import Data.Maybe
import Test.Hspec

import Service.Organization.OrganizationService

organizationServiceSpec =
  describe "Organization Service" $
    it "isValidGroupId" $ do
      isNothing (isValidGroupId "cz") `shouldBe` True
      isNothing (isValidGroupId "base.elixir") `shouldBe` True
      isNothing (isValidGroupId "base.elixir.e") `shouldBe` True
      isJust (isValidGroupId "a") `shouldBe` True
      isJust (isValidGroupId "a-b") `shouldBe` True
      isJust (isValidGroupId "a_bc") `shouldBe` True
      isJust (isValidGroupId ".cz") `shouldBe` True
      isJust (isValidGroupId "cz.") `shouldBe` True
      isJust (isValidGroupId "base.elixir.") `shouldBe` True
      isJust (isValidGroupId ".base.elixir") `shouldBe` True
      isJust (isValidGroupId "base.elixir-") `shouldBe` True

