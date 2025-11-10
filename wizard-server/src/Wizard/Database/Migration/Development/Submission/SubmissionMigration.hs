module Wizard.Database.Migration.Development.Submission.SubmissionMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Submission.SubmissionDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigSubmissionDAO
import Wizard.Database.Migration.Development.Submission.Data.Submissions
import Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs

runMigration = do
  logInfo _CMP_MIGRATION "(Submission/Submission) started"
  deleteTenantConfigSubmissions
  deleteSubmissions
  insertTenantConfigSubmission defaultSubmission
  insertSubmission submission1
  insertTenantConfigSubmission differentSubmission
  insertSubmission differentSubmission1
  logInfo _CMP_MIGRATION "(Submission/Submission) ended"
