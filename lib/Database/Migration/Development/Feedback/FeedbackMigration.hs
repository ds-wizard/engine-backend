module Database.Migration.Development.Feedback.FeedbackMigration where

import Database.DAO.Feedback.FeedbackDAO
import Database.Migration.Development.Feedback.Data.Feedbacks
import Util.Logger

runMigration = do
  logInfo "MIGRATION (Feedback/Feedback): started"
  deleteFeedbacks
  insertFeedback feedback1
  insertFeedback feedback2
  logInfo "MIGRATION (Feedback/Feedback): ended"
