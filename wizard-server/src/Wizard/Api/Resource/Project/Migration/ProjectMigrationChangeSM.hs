module Wizard.Api.Resource.Project.Migration.ProjectMigrationChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.Migration.ProjectMigrationChangeDTO
import Wizard.Api.Resource.Project.Migration.ProjectMigrationChangeJM ()
import Wizard.Database.Migration.Development.Project.Data.ProjectMigrations

instance ToSchema ProjectMigrationChangeDTO where
  declareNamedSchema = toSwagger projectMigrationChangeDto
