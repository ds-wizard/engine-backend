module Wizard.Specs.Service.Feedback.FeedbackServiceSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Database.Migration.Development.Feedback.Data.Feedbacks
import Wizard.Model.Config.AppConfig
import Wizard.Model.Feedback.Feedback
import Wizard.Service.Feedback.FeedbackService

import Wizard.Specs.Common

feedbackServiceIntegrationSpec appContext =
  describe "Feedback Service Integration" $ do
    describe "getFeedbacksFiltered" $
      it "Feedback is disabled" $
        -- GIVEN: Prepare expectations
        do
          let expectation = Left . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Feedback"
          -- AND: Update config in DB
          runInContext (modifyAppConfig (\c -> c {questionnaire = c.questionnaire {feedback = c.questionnaire.feedback {enabled = False}}})) appContext
          -- WHEN:
          result <- runInContext (getFeedbacksFiltered []) appContext
          -- THEN:
          result `shouldBe` expectation
    describe "createFeedback" $
      it "Feedback is disabled" $
        -- GIVEN: Prepare expectations
        do
          let expectation = Left . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Feedback"
          -- AND: Update config in DB
          runInContext (modifyAppConfig (\c -> c {questionnaire = c.questionnaire {feedback = c.questionnaire.feedback {enabled = False}}})) appContext
          -- WHEN:
          result <- runInContext (createFeedback feedback1Create) appContext
          -- THEN:
          result `shouldBe` expectation
    describe "createFeedbackWithGivenUuid" $
      it "Feedback is disabled" $
        -- GIVEN: Prepare expectations
        do
          let expectation = Left . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Feedback"
          -- AND: Update config in DB
          runInContext (modifyAppConfig (\c -> c {questionnaire = c.questionnaire {feedback = c.questionnaire.feedback {enabled = False}}})) appContext
          -- WHEN:
          result <- runInContext (createFeedbackWithGivenUuid feedback1.uuid feedback1Create) appContext
          -- THEN:
          result `shouldBe` expectation
    describe "getFeedbackByUuid" $
      it "Feedback is disabled" $
        -- GIVEN: Prepare expectations
        do
          let expectation = Left . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Feedback"
          -- AND: Update config in DB
          runInContext (modifyAppConfig (\c -> c {questionnaire = c.questionnaire {feedback = c.questionnaire.feedback {enabled = False}}})) appContext
          -- WHEN:
          result <- runInContext (getFeedbackByUuid feedback1.uuid) appContext
          -- THEN:
          result `shouldBe` expectation
