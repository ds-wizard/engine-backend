module Wizard.Api.Resource.KnowledgeModel.Editor.Event.SetRepliesDTO where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.QuestionnaireReply

data SetRepliesDTO = SetRepliesDTO
  { uuid :: U.UUID
  , replies :: M.Map String Reply
  }
  deriving (Show, Eq, Generic)
