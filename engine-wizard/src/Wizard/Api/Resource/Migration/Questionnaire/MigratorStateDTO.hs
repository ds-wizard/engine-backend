module Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO where

import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailDTO

data MigratorStateDTO =
  MigratorStateDTO
    { _migratorStateDTOOldQuestionnaire :: QuestionnaireDetailDTO
    , _migratorStateDTONewQuestionnaire :: QuestionnaireDetailDTO
    , _migratorStateDTOResolvedQuestionUuids :: [U.UUID]
    , _migratorStateDTOAppUuid :: U.UUID
    }
  deriving (Show, Eq, Generic)
