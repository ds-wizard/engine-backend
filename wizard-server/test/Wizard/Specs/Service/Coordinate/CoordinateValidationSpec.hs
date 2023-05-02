module Wizard.Specs.Service.Coordinate.CoordinateValidationSpec where

import Test.Hspec

import Wizard.Specs.Common
import WizardLib.Common.Service.Coordinate.CoordinateValidation

coordinateValidationSpec appContext =
  describe "Coordinate Validation" $ do
    it "validateCoordinateFormat" $ do
      shouldSucceed appContext (validateCoordinateFormat True "org.nl:core-nl:0.0.0")
      shouldSucceed appContext (validateCoordinateFormat False "org.nl:core-nl:0.0.0")
      shouldSucceed appContext (validateCoordinateFormat True "org.nl:core-nl:latest")
      shouldFailed appContext (validateCoordinateFormat False "org.nl:core-nl:latest")
      shouldFailed appContext (validateCoordinateFormat False "")
      shouldFailed appContext (validateCoordinateFormat False "0.0.0")
      shouldFailed appContext (validateCoordinateFormat False ":0.0.0")
      shouldFailed appContext (validateCoordinateFormat False "core-nl:0.0.0")
      shouldFailed appContext (validateCoordinateFormat False ":core-nl:0.0.0")
      shouldFailed appContext (validateCoordinateFormat False "org.nl::0.0.0")
      shouldFailed appContext (validateCoordinateFormat False "org.nl:core-nl:")
      shouldFailed appContext (validateCoordinateFormat False "org.nl:core-nl:1")
    it "validateVersionFormat" $ do
      shouldSucceed appContext (validateVersionFormat False "0.0.0")
      shouldSucceed appContext (validateVersionFormat False "1.2.0")
      shouldSucceed appContext (validateVersionFormat False "10.10.10")
      shouldSucceed appContext (validateVersionFormat True "100.100.100")
      shouldSucceed appContext (validateVersionFormat False "100.100.100")
      shouldSucceed appContext (validateVersionFormat True "latest")
      shouldFailed appContext (validateVersionFormat False "latest")
      shouldFailed appContext (validateVersionFormat False "1")
      shouldFailed appContext (validateVersionFormat False "1.")
      shouldFailed appContext (validateVersionFormat False "1.2")
      shouldFailed appContext (validateVersionFormat False "1.2.")
      shouldFailed appContext (validateVersionFormat False "1.2.a")
      shouldFailed appContext (validateVersionFormat False "1.2.3.4")
      shouldFailed appContext (validateVersionFormat False "a.2.3.4")
      shouldFailed appContext (validateVersionFormat False "a2.3.4")
      shouldFailed appContext (validateVersionFormat False "a.3.4")
    it "validateCoordinateWithParams" $ do
      shouldSucceed appContext (validateCoordinateWithParams "com:global:1.0.0" "com" "global" "1.0.0")
      shouldFailed appContext (validateCoordinateWithParams "" "com" "global" "1.0.0")
      shouldFailed appContext (validateCoordinateWithParams ":global:1.0.0" "com" "global" "1.0.0")
      shouldFailed appContext (validateCoordinateWithParams "com::1.0.0" "com" "global" "1.0.0")
      shouldFailed appContext (validateCoordinateWithParams "com:global:" "com" "global" "1.0.0")
      shouldFailed appContext (validateCoordinateWithParams "com:global:1.1.0" "com" "global" "1.0.0")
      shouldFailed appContext (validateCoordinateWithParams "com:global-2:1.1.0" "com" "global" "1.0.0")
