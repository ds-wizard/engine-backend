module Wizard.Api.Resource.Project.Migration.ProjectMigrationCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

data ProjectMigrationCreateDTO = ProjectMigrationCreateDTO
  { targetKnowledgeModelPackageUuid :: U.UUID
  , targetTagUuids :: [U.UUID]
  }
  deriving (Show, Eq, Generic)
