module Wizard.Api.Resource.Project.Migration.ProjectMigrationCreateDTO where

import qualified Data.UUID as U
import GHC.Generics

data ProjectMigrationCreateDTO = ProjectMigrationCreateDTO
  { targetKnowledgeModelPackageId :: String
  , targetTagUuids :: [U.UUID]
  }
  deriving (Show, Eq, Generic)
