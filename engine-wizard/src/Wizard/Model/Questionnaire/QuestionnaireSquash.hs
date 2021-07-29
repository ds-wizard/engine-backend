module Wizard.Model.Questionnaire.QuestionnaireSquash where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireVersion

data QuestionnaireSquash =
  QuestionnaireSquash
    { _questionnaireSquashUuid :: U.UUID
    , _questionnaireSquashEvents :: [QuestionnaireEvent]
    , _questionnaireSquashVersions :: [QuestionnaireVersion]
    }
  deriving (Generic, Show)
