module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCommentDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Model.DocumentTemplate.DocumentTemplateState
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Questionnaire.QuestionnaireState
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateDTO
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateFormatDTO
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

data QuestionnaireDetailDTO = QuestionnaireDetailDTO
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , phaseUuid :: Maybe U.UUID
  , visibility :: QuestionnaireVisibility
  , sharing :: QuestionnaireSharing
  , state :: QuestionnaireState
  , package :: PackageSimpleDTO
  , selectedQuestionTagUuids :: [U.UUID]
  , projectTags :: [String]
  , documentTemplateId :: Maybe String
  , documentTemplate :: Maybe DocumentTemplateDTO
  , formatUuid :: Maybe U.UUID
  , format :: Maybe DocumentTemplateFormatDTO
  , documentTemplateState :: Maybe DocumentTemplateState
  , documentTemplatePhase :: Maybe DocumentTemplatePhase
  , knowledgeModel :: KnowledgeModel
  , replies :: M.Map String Reply
  , commentThreadsMap :: M.Map String [QuestionnaireCommentThreadDTO]
  , labels :: M.Map String [U.UUID]
  , creatorUuid :: Maybe U.UUID
  , permissions :: [QuestionnairePermRecordDTO]
  , versions :: [QuestionnaireVersionDTO]
  , isTemplate :: Bool
  , migrationUuid :: Maybe U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq QuestionnaireDetailDTO where
  a == b =
    a.uuid == b.uuid
      && a.name == b.name
      && a.description == b.description
      && a.phaseUuid == b.phaseUuid
      && a.visibility == b.visibility
      && a.sharing == b.sharing
      && a.state == b.state
      && a.package == b.package
      && a.selectedQuestionTagUuids == b.selectedQuestionTagUuids
      && a.projectTags == b.projectTags
      && a.documentTemplateId == b.documentTemplateId
      && a.documentTemplate == b.documentTemplate
      && a.formatUuid == b.formatUuid
      && a.format == b.format
      && a.documentTemplateState == b.documentTemplateState
      && a.documentTemplatePhase == b.documentTemplatePhase
      && a.knowledgeModel == b.knowledgeModel
      && a.replies == b.replies
      && a.commentThreadsMap == b.commentThreadsMap
      && a.permissions == b.permissions
      && a.versions == b.versions
      && a.creatorUuid == b.creatorUuid
      && a.isTemplate == b.isTemplate
      && a.migrationUuid == b.migrationUuid
