module Wizard.Model.Migration.Questionnaire.MigratorState where

import qualified Data.UUID as U
import GHC.Generics

data MigratorState = MigratorState
  { oldQuestionnaireUuid :: U.UUID
  , newQuestionnaireUuid :: U.UUID
  , resolvedQuestionUuids :: [U.UUID]
  , appUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)
