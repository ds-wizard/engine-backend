module Wizard.Model.Questionnaire.QuestionnaireSquash where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireVersion

data QuestionnaireSquash = QuestionnaireSquash
  { uuid :: U.UUID
  , events :: [QuestionnaireEvent]
  , versions :: [QuestionnaireVersion]
  }
  deriving (Generic, Show)
