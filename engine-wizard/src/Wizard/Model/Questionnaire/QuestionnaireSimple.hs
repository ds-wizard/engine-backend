module Wizard.Model.Questionnaire.QuestionnaireSimple where

import qualified Data.UUID as U
import GHC.Generics

data QuestionnaireSimple =
  QuestionnaireSimple
    { _questionnaireSimpleUuid :: U.UUID
    , _questionnaireSimpleName :: String
    }
  deriving (Generic, Eq, Show)
