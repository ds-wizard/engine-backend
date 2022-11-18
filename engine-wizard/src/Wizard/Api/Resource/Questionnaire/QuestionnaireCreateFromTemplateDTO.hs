module Wizard.Api.Resource.Questionnaire.QuestionnaireCreateFromTemplateDTO where

import qualified Data.UUID as U
import GHC.Generics

data QuestionnaireCreateFromTemplateDTO = QuestionnaireCreateFromTemplateDTO
  { name :: String
  , questionnaireUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)
