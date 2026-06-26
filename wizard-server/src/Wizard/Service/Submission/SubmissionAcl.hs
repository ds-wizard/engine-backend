module Wizard.Service.Submission.SubmissionAcl where

import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.Service.Document.DocumentAcl

checkViewPermissionToSubmission :: Document -> AppContextM ()
checkViewPermissionToSubmission doc = do
  checkViewPermissionToDoc doc.projectUuid

checkEditPermissionToSubmission :: Document -> AppContextM ()
checkEditPermissionToSubmission doc = do
  checkEditPermissionToDoc doc.projectUuid
