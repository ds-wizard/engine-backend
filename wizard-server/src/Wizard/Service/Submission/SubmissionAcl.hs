module Wizard.Service.Submission.SubmissionAcl where

import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.Service.Document.DocumentAcl

checkViewPermissionToSubmission :: Document -> AppContextM ()
checkViewPermissionToSubmission doc = do
  checkPermission _SUBM_PERM
  checkViewPermissionToDoc doc.questionnaireUuid

checkEditPermissionToSubmission :: Document -> AppContextM ()
checkEditPermissionToSubmission doc = do
  checkPermission _SUBM_PERM
  checkEditPermissionToDoc doc.questionnaireUuid
