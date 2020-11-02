module Wizard.Specs.Service.Coordinate.CoordinateValidationSpec where

import Test.Hspec

import Wizard.Service.Coordinate.CoordinateValidation
import Wizard.Specs.Common

coordinateValidationSpec appContext =
  describe "Coordinate Validation" $ do
    it "validateCoordinateFormat" $ do
      shouldSucceed appContext (validateCoordinateFormat "org.nl:core-nl:0.0.0")
      shouldFailed appContext (validateCoordinateFormat "")
      shouldFailed appContext (validateCoordinateFormat "0.0.0")
      shouldFailed appContext (validateCoordinateFormat ":0.0.0")
      shouldFailed appContext (validateCoordinateFormat "core-nl:0.0.0")
      shouldFailed appContext (validateCoordinateFormat ":core-nl:0.0.0")
      shouldFailed appContext (validateCoordinateFormat "org.nl::0.0.0")
      shouldFailed appContext (validateCoordinateFormat "org.nl:core-nl:")
      shouldFailed appContext (validateCoordinateFormat "org.nl:core-nl:1")
    it "validateVersionFormat" $ do
      shouldSucceed appContext (validateVersionFormat "0.0.0")
      shouldSucceed appContext (validateVersionFormat "1.2.0")
      shouldSucceed appContext (validateVersionFormat "10.10.10")
      shouldSucceed appContext (validateVersionFormat "100.100.100")
      shouldFailed appContext (validateVersionFormat "1")
      shouldFailed appContext (validateVersionFormat "1.")
      shouldFailed appContext (validateVersionFormat "1.2")
      shouldFailed appContext (validateVersionFormat "1.2.")
      shouldFailed appContext (validateVersionFormat "1.2.a")
      shouldFailed appContext (validateVersionFormat "1.2.3.4")
      shouldFailed appContext (validateVersionFormat "a.2.3.4")
      shouldFailed appContext (validateVersionFormat "a2.3.4")
      shouldFailed appContext (validateVersionFormat "a.3.4")
    it "validateCoordinateWithParams" $ do
      shouldSucceed appContext (validateCoordinateWithParams "com:global:1.0.0" "com" "global" "1.0.0")
      shouldFailed appContext (validateCoordinateWithParams "" "com" "global" "1.0.0")
      shouldFailed appContext (validateCoordinateWithParams ":global:1.0.0" "com" "global" "1.0.0")
      shouldFailed appContext (validateCoordinateWithParams "com::1.0.0" "com" "global" "1.0.0")
      shouldFailed appContext (validateCoordinateWithParams "com:global:" "com" "global" "1.0.0")
      shouldFailed appContext (validateCoordinateWithParams "com:global:1.1.0" "com" "global" "1.0.0")
      shouldFailed appContext (validateCoordinateWithParams "com:global-2:1.1.0" "com" "global" "1.0.0")
