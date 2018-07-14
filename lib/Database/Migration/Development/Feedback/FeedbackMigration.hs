module Database.Migration.Development.Feedback.FeedbackMigration where

import Control.Monad.Logger (logInfo)

import Database.DAO.Feedback.FeedbackDAO
import Database.Migration.Development.Feedback.Data.Feedbacks

runMigration = do
  $(logInfo) "MIGRATION (Feedback/Feedback): started"
  deleteFeedbacks
  insertFeedback feedback1
  insertFeedback feedback2
  $(logInfo) "MIGRATION (Feedback/Feedback): ended"
