module Wizard.Model.Questionnaire.QuestionnaireCommentThreadAssigned where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import WizardLib.Public.Model.User.UserSuggestion

data QuestionnaireCommentThreadAssigned = QuestionnaireCommentThreadAssigned
  { questionnaireUuid :: U.UUID
  , questionnaireName :: String
  , commentThreadUuid :: U.UUID
  , path :: String
  , resolved :: Bool
  , private :: Bool
  , text :: String
  , createdBy :: Maybe UserSuggestion
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
