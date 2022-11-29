module Wizard.Api.Resource.Questionnaire.Version.QuestionnaireVersionRevertDTO where

import qualified Data.UUID as U
import GHC.Generics

data QuestionnaireVersionRevertDTO = QuestionnaireVersionRevertDTO
  { eventUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)
