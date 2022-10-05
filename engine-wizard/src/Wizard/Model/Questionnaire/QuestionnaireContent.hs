module Wizard.Model.Questionnaire.QuestionnaireContent where

import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.QuestionnaireReply

data QuestionnaireContent =
  QuestionnaireContent
    { _questionnaireContentPhaseUuid :: Maybe U.UUID
    , _questionnaireContentReplies :: M.Map String Reply
    , _questionnaireContentLabels :: M.Map String [U.UUID]
    }
  deriving (Generic, Eq, Show)
