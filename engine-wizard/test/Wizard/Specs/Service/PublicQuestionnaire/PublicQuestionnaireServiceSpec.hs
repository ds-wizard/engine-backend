module Wizard.Specs.Service.PublicQuestionnaire.PublicQuestionnaireServiceSpec where

import Control.Lens ((&), (.~))
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import LensesConfig
import Shared.Model.Error.Error
import Wizard.Localization.Messages.Public
import Wizard.Service.PublicQuestionnaire.PublicQuestionnaireService

import Wizard.Specs.Common

publicQuestionnaireServiceIntegrationSpec appContext =
  describe "PublicQuestionnaire Service Integration" $ do
    let updatedAppContext = appContext & applicationConfig . general . publicQuestionnaireEnabled .~ False
    describe "getPublicQuestionnaire" $ do
      it "PublicQuestionnaire is disabled" $
        -- GIVEN: Prepare expectations
       do
        let expectation = Left . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "PublicQuestionnaire"
        -- WHEN:
        result <- runInContext getPublicQuestionnaire updatedAppContext
        -- THEN:
        result `shouldBe` expectation
