module Registry.Specs.Service.Package.PackageValidationSpec where

import Data.Maybe
import Test.Hspec

import Registry.Service.Package.PackageValidation

packageValidationSpec =
  describe "Package Validation" $
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
