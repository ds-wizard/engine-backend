module Specs.Service.PublicQuestionnaire.PublicQuestionnaireServiceSpec where

import Control.Lens ((&), (.~))
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import LensesConfig
import Localization
import Model.Error.ErrorHelpers
import Service.PublicQuestionnaire.PublicQuestionnaireService

import Specs.Common

publicQuestionnaireServiceIntegrationSpec appContext =
  describe "PublicQuestionnaire Service Integration" $ do
    let updatedAppContext = appContext & appConfig . general . publicQuestionnaireEnabled .~ False
    describe "getPublicQuestionnaire" $ do
      it "PublicQuestionnaire is disabled" $
        -- GIVEN: Prepare expectations
       do
        let expectation =
              Left . createErrorWithErrorMessage . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "PublicQuestionnaire"
        -- WHEN:
        result <- runInContext getPublicQuestionnaire updatedAppContext
        -- THEN:
        result `shouldBe` expectation
