module Model.Questionnaire.Questionnaire where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Model.Questionnaire.QuestionnaireLabel
import Model.Questionnaire.QuestionnaireReply

data QuestionnaireAccessibility
  = PublicQuestionnaire
  | PrivateQuestionnaire
  | PublicReadOnlyQuestionnaire
  deriving (Show, Eq, Generic)

data Questionnaire = Questionnaire
  { _questionnaireUuid :: U.UUID
  , _questionnaireName :: String
  , _questionnaireLevel :: Int
  , _questionnaireAccessibility :: QuestionnaireAccessibility
  , _questionnairePackageId :: String
  , _questionnaireSelectedTagUuids :: [U.UUID]
  , _questionnaireOwnerUuid :: Maybe U.UUID
  , _questionnaireReplies :: [Reply]
  , _questionnaireLabels :: [Label]
  , _questionnaireCreatedAt :: UTCTime
  , _questionnaireUpdatedAt :: UTCTime
  } deriving (Generic, Show)

instance Eq Questionnaire where
  a == b =
    _questionnaireUuid a == _questionnaireUuid b &&
    _questionnaireName a == _questionnaireName b &&
    _questionnaireLevel a == _questionnaireLevel b &&
    _questionnaireAccessibility a == _questionnaireAccessibility b &&
    _questionnairePackageId a == _questionnairePackageId b &&
    _questionnaireSelectedTagUuids a == _questionnaireSelectedTagUuids b &&
    _questionnaireOwnerUuid a == _questionnaireOwnerUuid b &&
    _questionnaireReplies a == _questionnaireReplies b && _questionnaireLabels a == _questionnaireLabels b
