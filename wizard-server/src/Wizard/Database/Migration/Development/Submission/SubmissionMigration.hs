module Wizard.Database.Migration.Development.Submission.SubmissionMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Submission.SubmissionDAO
import Wizard.Database.Migration.Development.Submission.Data.Submissions

runMigration = do
  logInfo _CMP_MIGRATION "(Submission/Submission) started"
  deleteSubmissions
  insertSubmission submission1
  insertSubmission differentSubmission
  logInfo _CMP_MIGRATION "(Submission/Submission) ended"
