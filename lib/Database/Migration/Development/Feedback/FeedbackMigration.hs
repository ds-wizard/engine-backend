module Database.Migration.Development.Feedback.FeedbackMigration where

import Constant.Component
import Database.DAO.Feedback.FeedbackDAO
import Database.Migration.Development.Feedback.Data.Feedbacks
import Util.Logger

runMigration = do
  logInfo $ msg _CMP_MIGRATION "(Feedback/Feedback) started"
  deleteFeedbacks
  insertFeedback feedback1
  insertFeedback feedback2
  logInfo $ msg _CMP_MIGRATION "(Feedback/Feedback) ended"
