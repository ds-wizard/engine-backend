module Wizard.Model.Questionnaire.QuestionnaireSuggestion where

import qualified Data.UUID as U
import GHC.Generics

data QuestionnaireSuggestion = QuestionnaireSuggestion
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  }
  deriving (Show, Eq, Generic)
