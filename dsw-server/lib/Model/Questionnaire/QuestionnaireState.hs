module Model.Questionnaire.QuestionnaireState where

data QuestionnaireState
  = QSDefault
  | QSMigrating
  | QSOutdated
  deriving (Show, Eq)
