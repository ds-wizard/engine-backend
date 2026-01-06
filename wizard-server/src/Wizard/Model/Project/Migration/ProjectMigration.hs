module Wizard.Model.Project.Migration.ProjectMigration where

import qualified Data.UUID as U
import GHC.Generics

data ProjectMigration = ProjectMigration
  { oldProjectUuid :: U.UUID
  , newProjectUuid :: U.UUID
  , resolvedQuestionUuids :: [U.UUID]
  , tenantUuid :: U.UUID
  }
  deriving (Show, Eq, Generic)
