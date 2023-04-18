module Wizard.Model.Questionnaire.QuestionnaireState where

import GHC.Generics

data QuestionnaireState
  = QSDefault
  | QSMigrating
  | QSOutdated
  deriving (Show, Eq, Generic)
