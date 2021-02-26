module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Api.Resource.Template.TemplateDTO
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.Template.Template
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireAclDTO
import Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionDTO
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Questionnaire.QuestionnaireState

data QuestionnaireDetailDTO =
  QuestionnaireDetailDTO
    { _questionnaireDetailDTOUuid :: U.UUID
    , _questionnaireDetailDTOName :: String
    , _questionnaireDetailDTOLevel :: Int
    , _questionnaireDetailDTOVisibility :: QuestionnaireVisibility
    , _questionnaireDetailDTOSharing :: QuestionnaireSharing
    , _questionnaireDetailDTOState :: QuestionnaireState
    , _questionnaireDetailDTOPackage :: PackageSimpleDTO
    , _questionnaireDetailDTOSelectedTagUuids :: [U.UUID]
    , _questionnaireDetailDTOTemplateId :: Maybe String
    , _questionnaireDetailDTOTemplate :: Maybe TemplateDTO
    , _questionnaireDetailDTOFormatUuid :: Maybe U.UUID
    , _questionnaireDetailDTOFormat :: Maybe TemplateFormat
    , _questionnaireDetailDTOKnowledgeModel :: KnowledgeModel
    , _questionnaireDetailDTOReplies :: M.Map String Reply
    , _questionnaireDetailDTOLabels :: M.Map String [U.UUID]
    , _questionnaireDetailDTOCreatorUuid :: Maybe U.UUID
    , _questionnaireDetailDTOPermissions :: [QuestionnairePermRecordDTO]
    , _questionnaireDetailDTOEvents :: [QuestionnaireEventDTO]
    , _questionnaireDetailDTOVersions :: [QuestionnaireVersionDTO]
    , _questionnaireDetailDTOCreatedAt :: UTCTime
    , _questionnaireDetailDTOUpdatedAt :: UTCTime
    }
  deriving (Show, Generic)

instance Eq QuestionnaireDetailDTO where
  a == b =
    _questionnaireDetailDTOUuid a == _questionnaireDetailDTOUuid b &&
    _questionnaireDetailDTOName a == _questionnaireDetailDTOName b &&
    _questionnaireDetailDTOLevel a == _questionnaireDetailDTOLevel b &&
    _questionnaireDetailDTOVisibility a == _questionnaireDetailDTOVisibility b &&
    _questionnaireDetailDTOSharing a == _questionnaireDetailDTOSharing b &&
    _questionnaireDetailDTOState a == _questionnaireDetailDTOState b &&
    _questionnaireDetailDTOPackage a == _questionnaireDetailDTOPackage b &&
    _questionnaireDetailDTOSelectedTagUuids a == _questionnaireDetailDTOSelectedTagUuids b &&
    _questionnaireDetailDTOTemplateId a == _questionnaireDetailDTOTemplateId b &&
    _questionnaireDetailDTOTemplate a == _questionnaireDetailDTOTemplate b &&
    _questionnaireDetailDTOFormatUuid a == _questionnaireDetailDTOFormatUuid b &&
    _questionnaireDetailDTOFormat a == _questionnaireDetailDTOFormat b &&
    _questionnaireDetailDTOKnowledgeModel a == _questionnaireDetailDTOKnowledgeModel b &&
    _questionnaireDetailDTOReplies a == _questionnaireDetailDTOReplies b &&
    _questionnaireDetailDTOPermissions a == _questionnaireDetailDTOPermissions b &&
    _questionnaireDetailDTOEvents a == _questionnaireDetailDTOEvents b &&
    _questionnaireDetailDTOVersions a == _questionnaireDetailDTOVersions b &&
    _questionnaireDetailDTOCreatorUuid a == _questionnaireDetailDTOCreatorUuid b
