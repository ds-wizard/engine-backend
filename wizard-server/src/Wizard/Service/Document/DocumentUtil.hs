module Wizard.Service.Document.DocumentUtil where

import Data.Hashable
import qualified Data.Map.Strict as M
import qualified Data.UUID as U

import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.Model.Document.DocumentList
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Service.Document.DocumentMapper
import Wizard.Service.Submission.SubmissionService
import WizardLib.KnowledgeModel.Model.Event.Event

enhanceDocument :: TenantConfig -> DocumentList -> AppContextM DocumentDTO
enhanceDocument tenantConfig doc = do
  submissions <-
    if tenantConfig.submission.enabled
      then getSubmissionsForDocument doc.uuid
      else return []
  return $ toDTO doc submissions

filterAlreadyDoneDocument :: String -> U.UUID -> Document -> Bool
filterAlreadyDoneDocument documentTemplateId formatUuid doc =
  (doc.state == DoneDocumentState || doc.state == ErrorDocumentState) && Just doc.documentTemplateId == Just documentTemplateId && Just doc.formatUuid == Just formatUuid

computeHash :: [Event] -> Questionnaire -> Maybe U.UUID -> M.Map String Reply -> TenantConfig -> Maybe UserDTO -> Int
computeHash branchEvents qtn phaseUuid replies tenantConfig mCurrentUser =
  sum
    [ hash branchEvents
    , hash qtn.name
    , hash qtn.description
    , hash qtn.versions
    , hash qtn.projectTags
    , maybe 0 hash phaseUuid
    , hash . M.toList $ replies
    , hash tenantConfig.organization
    , maybe 0 hash mCurrentUser
    ]
