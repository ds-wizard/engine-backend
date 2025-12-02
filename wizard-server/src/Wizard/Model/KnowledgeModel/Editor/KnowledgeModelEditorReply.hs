module Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorReply where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.QuestionnaireReply
import WizardLib.Public.Model.User.UserSuggestion

data KnowledgeModelEditorReply = KnowledgeModelEditorReply
  { path :: String
  , value :: ReplyValue
  , knowledgeModelEditorUuid :: U.UUID
  , createdBy :: Maybe UserSuggestion
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
