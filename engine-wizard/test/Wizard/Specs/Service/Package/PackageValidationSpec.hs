module Wizard.Specs.Service.Package.PackageValidationSpec where

import Test.Hspec

import Wizard.Service.Package.PackageValidation
import Wizard.Specs.Common

packageValidationSpec appContext =
  describe "Package Validation" $ do
    it "validatePackageIdFormat" $ do
      shouldSucceed appContext (validatePackageIdFormat "org.nl:core-nl:0.0.0")
      shouldFailed appContext (validatePackageIdFormat "")
      shouldFailed appContext (validatePackageIdFormat "0.0.0")
      shouldFailed appContext (validatePackageIdFormat ":0.0.0")
      shouldFailed appContext (validatePackageIdFormat "core-nl:0.0.0")
      shouldFailed appContext (validatePackageIdFormat ":core-nl:0.0.0")
      shouldFailed appContext (validatePackageIdFormat "org.nl::0.0.0")
      shouldFailed appContext (validatePackageIdFormat "org.nl:core-nl:")
      shouldFailed appContext (validatePackageIdFormat "org.nl:core-nl:1")
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
    it "validateIsVersionHigher" $ do
      shouldSucceed appContext (validateIsVersionHigher "0.0.1" "0.0.0")
      shouldSucceed appContext (validateIsVersionHigher "0.1.0" "0.0.0")
      shouldSucceed appContext (validateIsVersionHigher "0.1.1" "0.0.0")
      shouldSucceed appContext (validateIsVersionHigher "1.0.0" "0.0.0")
      shouldSucceed appContext (validateIsVersionHigher "1.2.4" "1.2.3")
      shouldFailed appContext (validateIsVersionHigher "0.0.0" "0.0.0")
      shouldFailed appContext (validateIsVersionHigher "1.0.0" "1.0.0")
      shouldFailed appContext (validateIsVersionHigher "0.1.0" "1.0.0")
      shouldFailed appContext (validateIsVersionHigher "0.0.1" "1.0.0")
    it "validatePackageIdWithCoordinates" $ do
      shouldSucceed appContext (validatePackageIdWithCoordinates "com:global:1.0.0" "com" "global" "1.0.0")
      shouldFailed appContext (validatePackageIdWithCoordinates "" "com" "global" "1.0.0")
      shouldFailed appContext (validatePackageIdWithCoordinates ":global:1.0.0" "com" "global" "1.0.0")
      shouldFailed appContext (validatePackageIdWithCoordinates "com::1.0.0" "com" "global" "1.0.0")
      shouldFailed appContext (validatePackageIdWithCoordinates "com:global:" "com" "global" "1.0.0")
      shouldFailed appContext (validatePackageIdWithCoordinates "com:global:1.1.0" "com" "global" "1.0.0")
      shouldFailed appContext (validatePackageIdWithCoordinates "com:global-2:1.1.0" "com" "global" "1.0.0")
