module Specs.Service.Feedback.FeedbackServiceSpec where

import Control.Lens ((&), (.~), (^.))
import qualified Data.UUID as U
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Database.Migration.Development.Feedback.Data.Feedbacks
import LensesConfig
import Localization
import Model.Error.ErrorHelpers
import Service.Feedback.FeedbackService

import Specs.Common

feedbackServiceIntegrationSpec appContext =
  describe "Feedback Service Integration" $ do
    let updatedAppContext = appContext & appConfig . feedback . enabled .~ False
    describe "getFeedbacksFiltered" $ do
      it "Feedback is disabled" $
        -- GIVEN: Prepare expectations
       do
        let expectation = Left . createErrorWithErrorMessage . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Feedback"
        -- WHEN:
        result <- runInContext (getFeedbacksFiltered []) updatedAppContext
        -- THEN:
        result `shouldBe` expectation
    describe "createFeedback" $ do
      it "Feedback is disabled" $
        -- GIVEN: Prepare expectations
       do
        let expectation = Left . createErrorWithErrorMessage . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Feedback"
        -- WHEN:
        result <- runInContext (createFeedback feedback1Create) updatedAppContext
        -- THEN:
        result `shouldBe` expectation
    describe "createFeedbackWithGivenUuid" $ do
      it "Feedback is disabled" $
        -- GIVEN: Prepare expectations
       do
        let expectation = Left . createErrorWithErrorMessage . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Feedback"
        -- WHEN:
        result <- runInContext (createFeedbackWithGivenUuid (feedback1 ^. uuid) feedback1Create) updatedAppContext
        -- THEN:
        result `shouldBe` expectation
    describe "getFeedbackByUuid" $ do
      it "Feedback is disabled" $
        -- GIVEN: Prepare expectations
       do
        let expectation = Left . createErrorWithErrorMessage . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Feedback"
        -- WHEN:
        result <- runInContext (getFeedbackByUuid (U.toString $ feedback1 ^. uuid)) updatedAppContext
        -- THEN:
        result `shouldBe` expectation
    describe "synchronizeFeedbacks" $ do
      it "Feedback is disabled" $
        -- GIVEN: Prepare expectations
       do
        let expectation = Just . createErrorWithErrorMessage . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Feedback"
        -- WHEN:
        result <- runInContext synchronizeFeedbacks updatedAppContext
        -- THEN:
        result `shouldBe` expectation
