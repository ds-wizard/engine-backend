module Wizard.Api.Resource.Questionnaire.Migration.MigratorStateDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailQuestionnaireDTO

data MigratorStateDTO = MigratorStateDTO
  { oldQuestionnaire :: QuestionnaireDetailQuestionnaireDTO
  , newQuestionnaire :: QuestionnaireDetailQuestionnaireDTO
  , resolvedQuestionUuids :: [U.UUID]
  , tenantUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)
