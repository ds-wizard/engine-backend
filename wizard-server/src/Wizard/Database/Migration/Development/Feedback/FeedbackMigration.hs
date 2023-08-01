module Wizard.Database.Migration.Development.Feedback.FeedbackMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Feedback.FeedbackDAO
import Wizard.Database.Migration.Development.Feedback.Data.Feedbacks

runMigration = do
  logInfo _CMP_MIGRATION "(Feedback/Feedback) started"
  deleteFeedbacks
  insertFeedback feedback1
  insertFeedback feedback2
  insertFeedback differentFeedback
  logInfo _CMP_MIGRATION "(Feedback/Feedback) ended"
