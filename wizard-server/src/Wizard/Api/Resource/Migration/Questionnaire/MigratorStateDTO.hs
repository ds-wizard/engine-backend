module Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO

data MigratorStateDTO = MigratorStateDTO
  { oldQuestionnaire :: QuestionnaireDetailDTO
  , newQuestionnaire :: QuestionnaireDetailDTO
  , resolvedQuestionUuids :: [U.UUID]
  , tenantUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)
