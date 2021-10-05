module Wizard.Model.Questionnaire.QuestionnaireComment where

import Data.Hashable
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Util.Hashable ()

data QuestionnaireCommentThread =
  QuestionnaireCommentThread
    { _questionnaireCommentThreadUuid :: U.UUID
    , _questionnaireCommentThreadResolved :: Bool
    , _questionnaireCommentThreadComments :: [QuestionnaireComment]
    , _questionnaireCommentThreadPrivate :: Bool
    , _questionnaireCommentThreadCreatedBy :: Maybe UserSuggestionDTO
    , _questionnaireCommentThreadCreatedAt :: UTCTime
    , _questionnaireCommentThreadUpdatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

instance Hashable QuestionnaireCommentThread

data QuestionnaireComment =
  QuestionnaireComment
    { _questionnaireCommentUuid :: U.UUID
    , _questionnaireCommentText :: String
    , _questionnaireCommentCreatedBy :: Maybe UserSuggestionDTO
    , _questionnaireCommentCreatedAt :: UTCTime
    , _questionnaireCommentUpdatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

instance Hashable QuestionnaireComment
