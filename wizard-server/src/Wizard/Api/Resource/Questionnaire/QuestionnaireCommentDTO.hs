module Wizard.Api.Resource.Questionnaire.QuestionnaireCommentDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Util.Hashable ()
import WizardLib.Public.Api.Resource.User.UserSuggestionDTO

data QuestionnaireCommentThreadDTO = QuestionnaireCommentThreadDTO
  { uuid :: U.UUID
  , path :: String
  , resolved :: Bool
  , comments :: [QuestionnaireCommentDTO]
  , private :: Bool
  , createdBy :: Maybe UserSuggestionDTO
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data QuestionnaireCommentDTO = QuestionnaireCommentDTO
  { uuid :: U.UUID
  , text :: String
  , createdBy :: Maybe UserSuggestionDTO
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
