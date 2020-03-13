module Wizard.Specs.Service.PublicQuestionnaire.PublicQuestionnaireServiceSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import LensesConfig
import Shared.Model.Error.Error
import Wizard.Localization.Messages.Public
import Wizard.Service.PublicQuestionnaire.PublicQuestionnaireService

import Wizard.Specs.Common

publicQuestionnaireServiceIntegrationSpec appContext =
  describe "PublicQuestionnaire Service Integration" $
  describe "getPublicQuestionnaire" $
  it "PublicQuestionnaire is disabled" $
     -- GIVEN: Prepare expectations
   do
    let expectation = Left . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "PublicQuestionnaire"
     -- AND: Update config in DB
    runInContext (modifyAppConfig (features . publicQuestionnaire . enabled) False) appContext
     -- WHEN:
    result <- runInContext getPublicQuestionnaire appContext
     -- THEN:
    result `shouldBe` expectation
