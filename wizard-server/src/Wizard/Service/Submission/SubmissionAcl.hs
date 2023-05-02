module Wizard.Service.Submission.SubmissionAcl where

import qualified Data.UUID as U

import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.Service.Acl.AclService
import Wizard.Service.Document.DocumentAcl

checkPermissionToSubmission :: U.UUID -> AppContextM ()
checkPermissionToSubmission docUuid = do
  checkPermission _SUBM_PERM
  doc <- findDocumentByUuid docUuid
  checkEditPermissionToDoc doc.questionnaireUuid
