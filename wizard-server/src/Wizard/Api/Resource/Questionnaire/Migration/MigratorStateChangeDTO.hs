module Wizard.Api.Resource.Questionnaire.Migration.MigratorStateChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

data MigratorStateChangeDTO = MigratorStateChangeDTO
  { resolvedQuestionUuids :: [U.UUID]
  }
  deriving (Show, Eq, Generic)
