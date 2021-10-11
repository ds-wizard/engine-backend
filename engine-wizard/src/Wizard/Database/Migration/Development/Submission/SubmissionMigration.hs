module Wizard.Database.Migration.Development.Submission.SubmissionMigration where

import Shared.Constant.Component
import Wizard.Database.DAO.Submission.SubmissionDAO
import Wizard.Database.Migration.Development.Submission.Data.Submissions
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(Submission/Submission) started"
  deleteSubmissions
  insertSubmission submission1
  logInfo _CMP_MIGRATION "(Submission/Submission) ended"
