module Specs.API.Feedback.Common where

import Control.Lens ((^.))
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Api.Resource.Error.ErrorJM ()
import Database.DAO.Feedback.FeedbackDAO
import LensesConfig

import Specs.API.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfFeedbackInDB appContext feedback = do
  feedbackFromDb <- getFirstFromDB findFeedbacks appContext
  compareFeedbackDtos feedbackFromDb feedback

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareFeedbackDtos resDto expDto = do
  liftIO $ (resDto ^. questionUuid) `shouldBe` (expDto ^. questionUuid)
  liftIO $ (resDto ^. packageId) `shouldBe` (expDto ^. packageId)
  liftIO $ (resDto ^. title) `shouldBe` (expDto ^. title)
  liftIO $ (resDto ^. content) `shouldBe` (expDto ^. content)
