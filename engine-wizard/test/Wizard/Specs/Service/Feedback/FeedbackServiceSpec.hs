module Wizard.Specs.Service.Feedback.FeedbackServiceSpec where

import Control.Lens ((&), (.~), (^.))
import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import LensesConfig
import Shared.Model.Error.Error
import Wizard.Database.Migration.Development.Feedback.Data.Feedbacks
import Wizard.Localization.Messages.Public
import Wizard.Service.Feedback.FeedbackService

import Wizard.Specs.Common

feedbackServiceIntegrationSpec appContext =
  describe "Feedback Service Integration" $ do
    let updatedAppContext = appContext & applicationConfig . feedback . enabled .~ False
    describe "getFeedbacksFiltered" $ it "Feedback is disabled" $
        -- GIVEN: Prepare expectations
     do
      let expectation = Left . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Feedback"
        -- WHEN:
      result <- runInContext (getFeedbacksFiltered []) updatedAppContext
        -- THEN:
      result `shouldBe` expectation
    describe "createFeedback" $ it "Feedback is disabled" $
        -- GIVEN: Prepare expectations
     do
      let expectation = Left . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Feedback"
        -- WHEN:
      result <- runInContext (createFeedback feedback1Create) updatedAppContext
        -- THEN:
      result `shouldBe` expectation
    describe "createFeedbackWithGivenUuid" $ it "Feedback is disabled" $
        -- GIVEN: Prepare expectations
     do
      let expectation = Left . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Feedback"
        -- WHEN:
      result <- runInContext (createFeedbackWithGivenUuid (feedback1 ^. uuid) feedback1Create) updatedAppContext
        -- THEN:
      result `shouldBe` expectation
    describe "getFeedbackByUuid" $ it "Feedback is disabled" $
        -- GIVEN: Prepare expectations
     do
      let expectation = Left . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Feedback"
        -- WHEN:
      result <- runInContext (getFeedbackByUuid (U.toString $ feedback1 ^. uuid)) updatedAppContext
        -- THEN:
      result `shouldBe` expectation
    describe "synchronizeFeedbacks" $ it "Feedback is disabled" $
        -- GIVEN: Prepare expectations
     do
      let expectation = Left . UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Feedback"
        -- WHEN:
      result <- runInContext synchronizeFeedbacks updatedAppContext
        -- THEN:
      result `shouldBe` expectation
