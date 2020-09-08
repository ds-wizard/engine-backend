module Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Api.Resource.Package.PackageSimpleDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireReportDTO
import Wizard.Model.Questionnaire.Questionnaire
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
    , _questionnaireDetailDTOFormatUuid :: Maybe U.UUID
    , _questionnaireDetailDTOKnowledgeModel :: KnowledgeModel
    , _questionnaireDetailDTOReplies :: M.Map String ReplyValueDTO
    , _questionnaireDetailDTOLabels :: M.Map String [U.UUID]
    , _questionnaireDetailDTOOwnerUuid :: Maybe U.UUID
    , _questionnaireDetailDTOReport :: QuestionnaireReportDTO
    , _questionnaireDetailDTOCreatorUuid :: Maybe U.UUID
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
    _questionnaireDetailDTOFormatUuid a == _questionnaireDetailDTOFormatUuid b &&
    _questionnaireDetailDTOKnowledgeModel a == _questionnaireDetailDTOKnowledgeModel b &&
    _questionnaireDetailDTOReplies a == _questionnaireDetailDTOReplies b &&
    _questionnaireDetailDTOOwnerUuid a == _questionnaireDetailDTOOwnerUuid b &&
    _questionnaireDetailDTOReport a == _questionnaireDetailDTOReport b &&
    _questionnaireDetailDTOCreatorUuid a == _questionnaireDetailDTOCreatorUuid b
