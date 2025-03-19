module WizardLib.Specs.Common.Service.Coordinate.CoordinateValidationSpec where

import Data.Maybe (isJust, isNothing)
import Test.Hspec

import WizardLib.Common.Service.Coordinate.CoordinateValidation

coordinateValidationSpec =
  describe "Coordinate Validation" $ do
    it "isValidCoordinateFormat" $ do
      isNothing (isValidCoordinateFormat True "kmId" "org.nl:core-nl:0.0.0") `shouldBe` True
      isNothing (isValidCoordinateFormat False "kmId" "org.nl:core-nl:0.0.0") `shouldBe` True
      isNothing (isValidCoordinateFormat True "kmId" "org.nl:core-nl:latest") `shouldBe` True
      isJust (isValidCoordinateFormat False "kmId" "org.nl:core-nl:latest") `shouldBe` True
      isJust (isValidCoordinateFormat False "kmId" "") `shouldBe` True
      isJust (isValidCoordinateFormat False "kmId" "0.0.0") `shouldBe` True
      isJust (isValidCoordinateFormat False "kmId" ":0.0.0") `shouldBe` True
      isJust (isValidCoordinateFormat False "kmId" "core-nl:0.0.0") `shouldBe` True
      isJust (isValidCoordinateFormat False "kmId" ":core-nl:0.0.0") `shouldBe` True
      isJust (isValidCoordinateFormat False "kmId" "org.nl::0.0.0") `shouldBe` True
      isJust (isValidCoordinateFormat False "kmId" "org.nl:core-nl:") `shouldBe` True
      isJust (isValidCoordinateFormat False "kmId" "org.nl:core-nl:1") `shouldBe` True
    it "isValidVersionFormat" $ do
      isNothing (isValidVersionFormat False "0.0.0") `shouldBe` True
      isNothing (isValidVersionFormat False "1.2.0") `shouldBe` True
      isNothing (isValidVersionFormat False "10.10.10") `shouldBe` True
      isNothing (isValidVersionFormat True "100.100.100") `shouldBe` True
      isNothing (isValidVersionFormat False "100.100.100") `shouldBe` True
      isNothing (isValidVersionFormat True "latest") `shouldBe` True
      isJust (isValidVersionFormat False "latest") `shouldBe` True
      isJust (isValidVersionFormat False "1") `shouldBe` True
      isJust (isValidVersionFormat False "1.") `shouldBe` True
      isJust (isValidVersionFormat False "1.2") `shouldBe` True
      isJust (isValidVersionFormat False "1.2.") `shouldBe` True
      isJust (isValidVersionFormat False "1.2.a") `shouldBe` True
      isJust (isValidVersionFormat False "1.2.3.4") `shouldBe` True
      isJust (isValidVersionFormat False "a.2.3.4") `shouldBe` True
      isJust (isValidVersionFormat False "a2.3.4") `shouldBe` True
      isJust (isValidVersionFormat False "a.3.4") `shouldBe` True
    it "isValidCoordinateWithParams" $ do
      isNothing (isValidCoordinateWithParams "com:global:1.0.0" "com" "global" "1.0.0") `shouldBe` True
      isJust (isValidCoordinateWithParams "" "com" "global" "1.0.0") `shouldBe` True
      isJust (isValidCoordinateWithParams ":global:1.0.0" "com" "global" "1.0.0") `shouldBe` True
      isJust (isValidCoordinateWithParams "com::1.0.0" "com" "global" "1.0.0") `shouldBe` True
      isJust (isValidCoordinateWithParams "com:global:" "com" "global" "1.0.0") `shouldBe` True
      isJust (isValidCoordinateWithParams "com:global:1.1.0" "com" "global" "1.0.0") `shouldBe` True
      isJust (isValidCoordinateWithParams "com:global-2:1.1.0" "com" "global" "1.0.0") `shouldBe` True
    it "isValidCoordinatePartFormat" $ do
      isNothing (isValidCoordinatePartFormat "kmId" "core") `shouldBe` True
      isNothing (isValidCoordinatePartFormat "kmId" "ab") `shouldBe` True
      isNothing (isValidCoordinatePartFormat "kmId" "core-nl") `shouldBe` True
      isNothing (isValidCoordinatePartFormat "kmId" "core-nl-amsterdam") `shouldBe` True
      isNothing (isValidCoordinatePartFormat "kmId" "a") `shouldBe` True
      isNothing (isValidCoordinatePartFormat "kmId" "core.nl") `shouldBe` True
      isNothing (isValidCoordinatePartFormat "kmId" "a.b") `shouldBe` True
      isNothing (isValidCoordinatePartFormat "kmId" "core_nl") `shouldBe` True
      isJust (isValidCoordinatePartFormat "kmId" "code:nl") `shouldBe` True
      isJust (isValidCoordinatePartFormat "kmId" "code$") `shouldBe` True
      isJust (isValidCoordinatePartFormat "kmId" "") `shouldBe` True
