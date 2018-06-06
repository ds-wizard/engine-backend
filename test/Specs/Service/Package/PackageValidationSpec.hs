module Specs.Service.Package.PackageValidationSpec where

import Data.Maybe
import Test.Hspec

import Service.Package.PackageValidation

packageValidationSpec =
  describe "Package Validation" $ do
    it "validateVersionFormat" $ do
      isNothing (validateVersionFormat "0.0.0") `shouldBe` True
      isNothing (validateVersionFormat "1.2.0") `shouldBe` True
      isNothing (validateVersionFormat "10.10.10") `shouldBe` True
      isNothing (validateVersionFormat "100.100.100") `shouldBe` True
      isJust (validateVersionFormat "1") `shouldBe` True
      isJust (validateVersionFormat "1.") `shouldBe` True
      isJust (validateVersionFormat "1.2") `shouldBe` True
      isJust (validateVersionFormat "1.2.") `shouldBe` True
      isJust (validateVersionFormat "1.2.a") `shouldBe` True
      isJust (validateVersionFormat "1.2.3.4") `shouldBe` True
      isJust (validateVersionFormat "a.2.3.4") `shouldBe` True
      isJust (validateVersionFormat "a2.3.4") `shouldBe` True
      isJust (validateVersionFormat "a.3.4") `shouldBe` True
    it "validateIsVersionHigher" $ do
      isNothing (validateIsVersionHigher "0.0.1" "0.0.0") `shouldBe` True
      isNothing (validateIsVersionHigher "0.1.0" "0.0.0") `shouldBe` True
      isNothing (validateIsVersionHigher "0.1.1" "0.0.0") `shouldBe` True
      isNothing (validateIsVersionHigher "1.0.0" "0.0.0") `shouldBe` True
      isNothing (validateIsVersionHigher "1.2.4" "1.2.3") `shouldBe` True
      isJust (validateIsVersionHigher "0.0.0" "0.0.0") `shouldBe` True
      isJust (validateIsVersionHigher "1.0.0" "1.0.0") `shouldBe` True
      isJust (validateIsVersionHigher "0.1.0" "1.0.0") `shouldBe` True
      isJust (validateIsVersionHigher "0.0.1" "1.0.0") `shouldBe` True
