module Wizard.Api.Resource.Questionnaire.QuestionnaireContentDTO where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.Event.QuestionnaireEventDTO
import Wizard.Model.Questionnaire.QuestionnaireCommentList
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Questionnaire.QuestionnaireVersionList

data QuestionnaireContentDTO = QuestionnaireContentDTO
  { phaseUuid :: Maybe U.UUID
  , replies :: M.Map String Reply
  , commentThreadsMap :: M.Map String [QuestionnaireCommentThreadList]
  , labels :: M.Map String [U.UUID]
  , events :: [QuestionnaireEventDTO]
  , versions :: [QuestionnaireVersionList]
  }
  deriving (Show, Eq, Generic)
