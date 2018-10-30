module Model.Questionnaire.Questionnaire where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Model.KnowledgeModel.KnowledgeModel

data QuestionnaireReply = QuestionnaireReply
  { _questionnaireReplyPath :: String
  , _questionnaireReplyValue :: String
  } deriving (Generic, Show, Eq)

data Questionnaire = Questionnaire
  { _questionnaireUuid :: U.UUID
  , _questionnaireName :: String
  , _questionnaireLevel :: Int
  , _questionnairePrivate :: Bool
  , _questionnairePackageId :: String
  , _questionnaireKnowledgeModel :: KnowledgeModel
  , _questionnaireOwnerUuid :: Maybe U.UUID
  , _questionnaireReplies :: [QuestionnaireReply]
  , _questionnaireCreatedAt :: UTCTime
  , _questionnaireUpdatedAt :: UTCTime
  } deriving (Generic, Show, Eq)
