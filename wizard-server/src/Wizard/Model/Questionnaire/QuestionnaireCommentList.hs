module Wizard.Model.Questionnaire.QuestionnaireCommentList where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import WizardLib.Public.Model.User.UserSuggestion

data QuestionnaireCommentThreadList = QuestionnaireCommentThreadList
  { uuid :: U.UUID
  , path :: String
  , resolved :: Bool
  , comments :: [QuestionnaireCommentList]
  , private :: Bool
  , assignedTo :: Maybe UserSuggestion
  , createdBy :: Maybe UserSuggestion
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

data QuestionnaireCommentList = QuestionnaireCommentList
  { uuid :: U.UUID
  , text :: String
  , createdBy :: Maybe UserSuggestion
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance Ord QuestionnaireCommentList where
  compare a b = compare a.uuid b.uuid
