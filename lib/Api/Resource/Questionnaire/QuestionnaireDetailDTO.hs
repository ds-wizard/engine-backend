module Api.Resource.Questionnaire.QuestionnaireDetailDTO where

import Data.Time
import qualified Data.UUID as U

import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Api.Resource.Package.PackageSimpleDTO
import Api.Resource.Questionnaire.QuestionnaireReplyDTO
import Api.Resource.Questionnaire.QuestionnaireReplyJS ()
import Model.Questionnaire.Questionnaire
import Model.Questionnaire.QuestionnaireState

data QuestionnaireDetailDTO = QuestionnaireDetailDTO
  { _questionnaireDetailDTOUuid :: U.UUID
  , _questionnaireDetailDTOName :: String
  , _questionnaireDetailDTOLevel :: Int
  , _questionnaireDetailDTOAccessibility :: QuestionnaireAccessibility
  , _questionnaireDetailDTOState :: QuestionnaireState
  , _questionnaireDetailDTOPackage :: PackageSimpleDTO
  , _questionnaireDetailDTOSelectedTagUuids :: [U.UUID]
  , _questionnaireDetailDTOKnowledgeModel :: KnowledgeModelDTO
  , _questionnaireDetailDTOReplies :: [ReplyDTO]
  , _questionnaireDetailDTOOwnerUuid :: Maybe U.UUID
  , _questionnaireDetailDTOCreatedAt :: UTCTime
  , _questionnaireDetailDTOUpdatedAt :: UTCTime
  } deriving (Show)

instance Eq QuestionnaireDetailDTO where
  a == b =
    _questionnaireDetailDTOUuid a == _questionnaireDetailDTOUuid b &&
    _questionnaireDetailDTOName a == _questionnaireDetailDTOName b &&
    _questionnaireDetailDTOLevel a == _questionnaireDetailDTOLevel b &&
    _questionnaireDetailDTOAccessibility a == _questionnaireDetailDTOAccessibility b &&
    _questionnaireDetailDTOState a == _questionnaireDetailDTOState b &&
    _questionnaireDetailDTOPackage a == _questionnaireDetailDTOPackage b &&
    _questionnaireDetailDTOSelectedTagUuids a == _questionnaireDetailDTOSelectedTagUuids b &&
    _questionnaireDetailDTOKnowledgeModel a == _questionnaireDetailDTOKnowledgeModel b &&
    _questionnaireDetailDTOReplies a == _questionnaireDetailDTOReplies b &&
    _questionnaireDetailDTOOwnerUuid a == _questionnaireDetailDTOOwnerUuid b
