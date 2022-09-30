module Wizard.Model.Questionnaire.QuestionnaireComment where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data QuestionnaireCommentThread =
  QuestionnaireCommentThread
    { _questionnaireCommentThreadUuid :: U.UUID
    , _questionnaireCommentThreadPath :: String
    , _questionnaireCommentThreadResolved :: Bool
    , _questionnaireCommentThreadComments :: [QuestionnaireComment]
    , _questionnaireCommentThreadPrivate :: Bool
    , _questionnaireCommentThreadQuestionnaireUuid :: U.UUID
    , _questionnaireCommentThreadCreatedBy :: Maybe U.UUID
    , _questionnaireCommentThreadCreatedAt :: UTCTime
    , _questionnaireCommentThreadUpdatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

data QuestionnaireComment =
  QuestionnaireComment
    { _questionnaireCommentUuid :: U.UUID
    , _questionnaireCommentText :: String
    , _questionnaireCommentThreadUuid :: U.UUID
    , _questionnaireCommentCreatedBy :: Maybe U.UUID
    , _questionnaireCommentCreatedAt :: UTCTime
    , _questionnaireCommentUpdatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)
