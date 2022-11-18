module Wizard.Api.Resource.Migration.Questionnaire.MigratorStateChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

data MigratorStateChangeDTO = MigratorStateChangeDTO
  { resolvedQuestionUuids :: [U.UUID]
  }
  deriving (Show, Eq, Generic)
