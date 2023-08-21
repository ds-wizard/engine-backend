module Wizard.Specs.Service.User.UserServiceSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Config.SimpleFeature
import Shared.Common.Model.Error.Error
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Config.AppConfig
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
          runInContext (modifyAppConfig (\c -> c {authentication = c.authentication {internal = c.authentication.internal {registration = c.authentication.internal.registration {enabled = False}}}})) appContext
          -- WHEN:
          result <- runInContext (registerUser userJohnCreate) appContext
          -- THEN:
          result `shouldBe` expectation
