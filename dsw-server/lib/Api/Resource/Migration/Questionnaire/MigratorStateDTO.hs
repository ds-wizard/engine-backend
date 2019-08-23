module Api.Resource.Migration.Questionnaire.MigratorStateDTO where

import qualified Data.UUID as U
import GHC.Generics

import Api.Resource.Questionnaire.QuestionnaireDetailDTO

data MigratorStateDTO = MigratorStateDTO
  { _migratorStateDTOOldQuestionnaire :: QuestionnaireDetailDTO
  , _migratorStateDTONewQuestionnaire :: QuestionnaireDetailDTO
  , _migratorStateDTOResolvedQuestionUuids :: [U.UUID]
  } deriving (Show, Eq, Generic)
