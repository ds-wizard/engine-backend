module Wizard.Model.Questionnaire.QuestionnaireSimpleWithPerm where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnairePerm

data QuestionnaireSimpleWithPerm = QuestionnaireSimpleWithPerm
  { uuid :: U.UUID
  , visibility :: QuestionnaireVisibility
  , sharing :: QuestionnaireSharing
  , tenantUuid :: U.UUID
  , permissions :: [QuestionnairePerm]
  }
  deriving (Generic, Eq, Show)
