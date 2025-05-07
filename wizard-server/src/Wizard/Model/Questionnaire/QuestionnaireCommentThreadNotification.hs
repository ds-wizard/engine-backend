module Wizard.Model.Questionnaire.QuestionnaireCommentThreadNotification where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.User.UserSuggestion

data QuestionnaireCommentThreadNotification = QuestionnaireCommentThreadNotification
  { questionnaireUuid :: U.UUID
  , questionnaireName :: String
  , tenantUuid :: U.UUID
  , commentThreadUuid :: U.UUID
  , path :: String
  , resolved :: Bool
  , private :: Bool
  , assignedTo :: UserSuggestion
  , assignedBy :: Maybe UserSuggestion
  , text :: String
  , clientUrl :: String
  , appTitle :: Maybe String
  , logoUrl :: Maybe String
  , primaryColor :: Maybe String
  , illustrationsColor :: Maybe String
  , supportEmail :: Maybe String
  , mailConfigUuid :: Maybe U.UUID
  }
  deriving (Show, Eq, Generic)

instance Ord QuestionnaireCommentThreadNotification where
  compare a b = compare a.questionnaireUuid b.questionnaireUuid
