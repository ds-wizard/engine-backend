module Wizard.Api.Resource.Project.Migration.ProjectMigrationChangeDTO where

import qualified Data.UUID as U
import GHC.Generics

data ProjectMigrationChangeDTO = ProjectMigrationChangeDTO
  { resolvedQuestionUuids :: [U.UUID]
  }
  deriving (Show, Eq, Generic)
