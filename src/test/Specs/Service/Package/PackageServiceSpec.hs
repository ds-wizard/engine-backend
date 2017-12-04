module Specs.Service.Package.PackageServiceSpec where

import Data.Maybe
import Test.Hspec

import Service.Package.PackageService

packageServiceSpec =
  describe "Package Service" $ do
    it "isVersionInValidFormat" $ do
      isNothing (isVersionInValidFormat "0.0.0") `shouldBe` True
      isNothing (isVersionInValidFormat "1.2.0") `shouldBe` True
      isNothing (isVersionInValidFormat "10.10.10") `shouldBe` True
      isNothing (isVersionInValidFormat "100.100.100") `shouldBe` True
      isJust (isVersionInValidFormat "1") `shouldBe` True
      isJust (isVersionInValidFormat "1.") `shouldBe` True
      isJust (isVersionInValidFormat "1.2") `shouldBe` True
      isJust (isVersionInValidFormat "1.2.") `shouldBe` True
      isJust (isVersionInValidFormat "1.2.a") `shouldBe` True
      isJust (isVersionInValidFormat "1.2.3.4") `shouldBe` True
      isJust (isVersionInValidFormat "a.2.3.4") `shouldBe` True
      isJust (isVersionInValidFormat "a2.3.4") `shouldBe` True
      isJust (isVersionInValidFormat "a.3.4") `shouldBe` True
    it "isVersionHigher" $ do
      isNothing (isVersionHigher "0.0.1" "0.0.0") `shouldBe` True
      isNothing (isVersionHigher "0.1.0" "0.0.0") `shouldBe` True
      isNothing (isVersionHigher "0.1.1" "0.0.0") `shouldBe` True
      isNothing (isVersionHigher "1.0.0" "0.0.0") `shouldBe` True
      isNothing (isVersionHigher "1.2.4" "1.2.3") `shouldBe` True
      isJust (isVersionHigher "0.0.0" "0.0.0") `shouldBe` True
      isJust (isVersionHigher "1.0.0" "1.0.0") `shouldBe` True
      isJust (isVersionHigher "0.1.0" "1.0.0") `shouldBe` True
      isJust (isVersionHigher "0.0.1" "1.0.0") `shouldBe` True
