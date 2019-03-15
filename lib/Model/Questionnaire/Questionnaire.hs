module Model.Questionnaire.Questionnaire where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Model.Questionnaire.QuestionnaireReply

data Questionnaire = Questionnaire
  { _questionnaireUuid :: U.UUID
  , _questionnaireName :: String
  , _questionnaireLevel :: Int
  , _questionnairePrivate :: Bool
  , _questionnairePackageId :: String
  , _questionnaireSelectedTagUuids :: [U.UUID]
  , _questionnaireOwnerUuid :: Maybe U.UUID
  , _questionnaireReplies :: [Reply]
  , _questionnaireCreatedAt :: UTCTime
  , _questionnaireUpdatedAt :: UTCTime
  } deriving (Generic, Show)

instance Eq Questionnaire where
  a == b =
    _questionnaireUuid a == _questionnaireUuid b &&
    _questionnaireName a == _questionnaireName b &&
    _questionnaireLevel a == _questionnaireLevel b &&
    _questionnairePrivate a == _questionnairePrivate b &&
    _questionnairePackageId a == _questionnairePackageId b &&
    _questionnaireSelectedTagUuids a == _questionnaireSelectedTagUuids b &&
    _questionnaireOwnerUuid a == _questionnaireOwnerUuid b && _questionnaireReplies a == _questionnaireReplies b
