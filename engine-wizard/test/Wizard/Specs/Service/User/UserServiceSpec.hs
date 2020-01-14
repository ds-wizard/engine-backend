module Wizard.Specs.Service.User.UserServiceSpec where

import Control.Lens ((&), (.~))
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import LensesConfig
import Shared.Model.Error.Error
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Localization.Messages.Public
import Wizard.Service.User.UserService

import Wizard.Specs.Common

userServiceIntegrationSpec appContext =
  describe "User Service Integration" $ do
    let updatedAppContext = appContext & applicationConfig . general . registrationEnabled .~ False
    describe "registrateUser" $ do
      it "Registation is disabled" $
        -- GIVEN: Prepare expectations
       do
        let expectation = Left . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Registration"
        -- WHEN:
        result <- runInContext (registrateUser userJohnCreate) updatedAppContext
        -- THEN:
        result `shouldBe` expectation
