module Specs.Service.User.UserServiceSpec where

import Control.Lens ((&), (.~))
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Database.Migration.Development.User.Data.Users
import LensesConfig
import Localization
import Model.Error.ErrorHelpers
import Service.User.UserService

import Specs.Common

userServiceIntegrationSpec appContext =
  describe "User Service Integration" $ do
    let updatedAppContext = appContext & appConfig . general . registrationEnabled .~ False
    describe "registrateUser" $ do
      it "Registation is disabled" $
        -- GIVEN: Prepare expectations
       do
        let expectation =
              Left . createErrorWithErrorMessage . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Registration"
        -- WHEN:
        result <- runInContext (registrateUser userJohnCreate) updatedAppContext
        -- THEN:
        result `shouldBe` expectation
