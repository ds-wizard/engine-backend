module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.Template.TemplateDTO
import Shared.Api.Resource.Template.TemplateFormatDTO
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireComment
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Model.Template.TemplateState

data QuestionnaireDetailDTO =
  QuestionnaireDetailDTO
    { _questionnaireDetailDTOUuid :: U.UUID
    , _questionnaireDetailDTOName :: String
    , _questionnaireDetailDTODescription :: Maybe String
    , _questionnaireDetailDTOPhaseUuid :: Maybe U.UUID
    , _questionnaireDetailDTOVisibility :: QuestionnaireVisibility
    , _questionnaireDetailDTOSharing :: QuestionnaireSharing
    , _questionnaireDetailDTOState :: QuestionnaireState
    , _questionnaireDetailDTOPackage :: PackageSimpleDTO
    , _questionnaireDetailDTOSelectedQuestionTagUuids :: [U.UUID]
    , _questionnaireDetailDTOProjectTags :: [String]
    , _questionnaireDetailDTOTemplateId :: Maybe String
    , _questionnaireDetailDTOTemplate :: Maybe TemplateDTO
    , _questionnaireDetailDTOFormatUuid :: Maybe U.UUID
    , _questionnaireDetailDTOFormat :: Maybe TemplateFormatDTO
    , _questionnaireDetailDTOTemplateState :: Maybe TemplateState
    , _questionnaireDetailDTOKnowledgeModel :: KnowledgeModel
    , _questionnaireDetailDTOReplies :: M.Map String Reply
    , _questionnaireDetailDTOCommentThreadsMap :: M.Map String [QuestionnaireCommentThread]
    , _questionnaireDetailDTOLabels :: M.Map String [U.UUID]
    , _questionnaireDetailDTOCreatorUuid :: Maybe U.UUID
    , _questionnaireDetailDTOPermissions :: [QuestionnairePermRecordDTO]
    , _questionnaireDetailDTOEvents :: [QuestionnaireEventDTO]
    , _questionnaireDetailDTOVersions :: [QuestionnaireVersionDTO]
    , _questionnaireDetailDTOIsTemplate :: Bool
    , _questionnaireDetailDTOMigrationUuid :: Maybe U.UUID
    , _questionnaireDetailDTOCreatedAt :: UTCTime
    , _questionnaireDetailDTOUpdatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq QuestionnaireDetailDTO where
  a == b =
    _questionnaireDetailDTOUuid a == _questionnaireDetailDTOUuid b &&
    _questionnaireDetailDTOName a == _questionnaireDetailDTOName b &&
    _questionnaireDetailDTODescription a == _questionnaireDetailDTODescription b &&
    _questionnaireDetailDTOPhaseUuid a == _questionnaireDetailDTOPhaseUuid b &&
    _questionnaireDetailDTOVisibility a == _questionnaireDetailDTOVisibility b &&
    _questionnaireDetailDTOSharing a == _questionnaireDetailDTOSharing b &&
    _questionnaireDetailDTOState a == _questionnaireDetailDTOState b &&
    _questionnaireDetailDTOPackage a == _questionnaireDetailDTOPackage b &&
    _questionnaireDetailDTOSelectedQuestionTagUuids a == _questionnaireDetailDTOSelectedQuestionTagUuids b &&
    _questionnaireDetailDTOProjectTags a == _questionnaireDetailDTOProjectTags b &&
    _questionnaireDetailDTOTemplateId a == _questionnaireDetailDTOTemplateId b &&
    _questionnaireDetailDTOTemplate a == _questionnaireDetailDTOTemplate b &&
    _questionnaireDetailDTOFormatUuid a == _questionnaireDetailDTOFormatUuid b &&
    _questionnaireDetailDTOFormat a == _questionnaireDetailDTOFormat b &&
    _questionnaireDetailDTOTemplateState a == _questionnaireDetailDTOTemplateState b &&
    _questionnaireDetailDTOKnowledgeModel a == _questionnaireDetailDTOKnowledgeModel b &&
    _questionnaireDetailDTOReplies a == _questionnaireDetailDTOReplies b &&
    _questionnaireDetailDTOCommentThreadsMap a == _questionnaireDetailDTOCommentThreadsMap b &&
    _questionnaireDetailDTOPermissions a == _questionnaireDetailDTOPermissions b &&
    _questionnaireDetailDTOEvents a == _questionnaireDetailDTOEvents b &&
    _questionnaireDetailDTOVersions a == _questionnaireDetailDTOVersions b &&
    _questionnaireDetailDTOCreatorUuid a == _questionnaireDetailDTOCreatorUuid b &&
    _questionnaireDetailDTOIsTemplate a == _questionnaireDetailDTOIsTemplate b &&
    _questionnaireDetailDTOMigrationUuid a == _questionnaireDetailDTOMigrationUuid b
