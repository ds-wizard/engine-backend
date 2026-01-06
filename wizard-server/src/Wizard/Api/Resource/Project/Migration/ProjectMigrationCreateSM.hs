module Wizard.Api.Resource.Project.Migration.ProjectMigrationCreateSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.Migration.ProjectMigrationCreateDTO
import Wizard.Api.Resource.Project.Migration.ProjectMigrationCreateJM ()
import Wizard.Database.Migration.Development.Project.Data.ProjectMigrations

instance ToSchema ProjectMigrationCreateDTO where
  declareNamedSchema = toSwagger projectMigrationCreateDto
