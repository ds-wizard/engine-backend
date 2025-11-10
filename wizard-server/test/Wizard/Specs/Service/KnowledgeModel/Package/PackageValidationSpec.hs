module Wizard.Specs.Service.KnowledgeModel.Package.PackageValidationSpec where

import Test.Hspec

import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageValidation
import Wizard.Specs.Common

packageValidationSpec appContext =
  describe "Package Validation" $
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
