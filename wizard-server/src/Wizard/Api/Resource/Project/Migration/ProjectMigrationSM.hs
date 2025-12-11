module Wizard.Api.Resource.Project.Migration.ProjectMigrationSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireSM ()
import Wizard.Api.Resource.Project.Migration.ProjectMigrationDTO
import Wizard.Api.Resource.Project.Migration.ProjectMigrationJM ()
import Wizard.Database.Migration.Development.Project.Data.ProjectMigrations

instance ToSchema ProjectMigrationDTO where
  declareNamedSchema = toSwagger projectMigrationDto
