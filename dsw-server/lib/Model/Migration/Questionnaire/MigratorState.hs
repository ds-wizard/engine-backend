module Model.Migration.Questionnaire.MigratorState where

import qualified Data.UUID as U
import GHC.Generics

data MigratorState = MigratorState
  { _migratorStateOldQuestionnaireUuid :: U.UUID
  , _migratorStateNewQuestionnaireUuid :: U.UUID
  , _migratorStateResolvedQuestionUuids :: [U.UUID]
  } deriving (Show, Eq, Generic)
