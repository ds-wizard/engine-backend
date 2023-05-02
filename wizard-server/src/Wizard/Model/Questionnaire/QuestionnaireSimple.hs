module Wizard.Model.Questionnaire.QuestionnaireSimple where

import qualified Data.UUID as U
import GHC.Generics

data QuestionnaireSimple = QuestionnaireSimple
  { uuid :: U.UUID
  , name :: String
  }
  deriving (Generic, Eq, Show)
