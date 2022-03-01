module Wizard.Specs.Service.User.UserServiceSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import LensesConfig
import Shared.Model.Error.Error
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Localization.Messages.Public
import Wizard.Service.User.UserService

import Wizard.Specs.Common

userServiceIntegrationSpec appContext =
  describe "User Service Integration" $
  describe "registerUser" $
  it "Registation is disabled" $
     -- GIVEN: Prepare expectations
   do
    let expectation = Left . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Registration"
     -- AND: Update config in DB
    runInContext (modifyAppConfig (authentication . internal . registration . enabled) False) appContext
     -- WHEN:
    result <- runInContext (registerUser userJohnCreate) appContext
     -- THEN:
    result `shouldBe` expectation
