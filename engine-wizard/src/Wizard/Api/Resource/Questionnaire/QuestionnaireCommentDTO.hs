module Wizard.Api.Resource.Questionnaire.QuestionnaireCommentDTO where

import Data.Hashable
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.User.UserSuggestionDTO
import Wizard.Util.Hashable ()

data QuestionnaireCommentThreadDTO =
  QuestionnaireCommentThreadDTO
    { _questionnaireCommentThreadDTOUuid :: U.UUID
    , _questionnaireCommentThreadDTOPath :: String
    , _questionnaireCommentThreadDTOResolved :: Bool
    , _questionnaireCommentThreadDTOComments :: [QuestionnaireCommentDTO]
    , _questionnaireCommentThreadDTOPrivate :: Bool
    , _questionnaireCommentThreadDTOCreatedBy :: Maybe UserSuggestionDTO
    , _questionnaireCommentThreadDTOCreatedAt :: UTCTime
    , _questionnaireCommentThreadDTOUpdatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

instance Hashable QuestionnaireCommentThreadDTO

data QuestionnaireCommentDTO =
  QuestionnaireCommentDTO
    { _questionnaireCommentDTOUuid :: U.UUID
    , _questionnaireCommentDTOText :: String
    , _questionnaireCommentDTOCreatedBy :: Maybe UserSuggestionDTO
    , _questionnaireCommentDTOCreatedAt :: UTCTime
    , _questionnaireCommentDTOUpdatedAt :: UTCTime
    }
  deriving (Show, Eq, Generic)

instance Hashable QuestionnaireCommentDTO
