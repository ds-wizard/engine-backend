module Wizard.Specs.Service.User.UserServiceSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Config.SimpleFeature
import Shared.Common.Model.Error.Error
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.Tenant.Config.ConfigService
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
          (Right tcAuthentication) <- runInContext getCurrentTenantConfigAuthentication appContext
          let tcAuthenticationUpdated = tcAuthentication {internal = tcAuthentication.internal {registration = tcAuthentication.internal.registration {enabled = False}}}
          tcAuthentication <- runInContext (modifyTenantConfigAuthentication tcAuthenticationUpdated) appContext
          -- WHEN:
          result <- runInContext (registerUser userJohnCreate) appContext
          -- THEN:
          result `shouldBe` expectation
