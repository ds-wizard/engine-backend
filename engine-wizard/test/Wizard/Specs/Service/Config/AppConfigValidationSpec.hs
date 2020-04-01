module Wizard.Specs.Service.Config.AppConfigValidationSpec where

import Data.Maybe
import Test.Hspec

import Wizard.Service.Config.AppConfigValidation

appConfigValidationSpec =
  describe "AppConfigValidation" $
  it "isValidOrganizationId" $ do
    isNothing (isValidOrganizationId "cz") `shouldBe` True
    isNothing (isValidOrganizationId "base.global") `shouldBe` True
    isNothing (isValidOrganizationId "base.global.e") `shouldBe` True
    isJust (isValidOrganizationId "a") `shouldBe` True
    isJust (isValidOrganizationId "a-b") `shouldBe` True
    isJust (isValidOrganizationId "a_bc") `shouldBe` True
    isJust (isValidOrganizationId ".cz") `shouldBe` True
    isJust (isValidOrganizationId "cz.") `shouldBe` True
    isJust (isValidOrganizationId "base.global.") `shouldBe` True
    isJust (isValidOrganizationId ".base.global") `shouldBe` True
    isJust (isValidOrganizationId "base.global-") `shouldBe` True
