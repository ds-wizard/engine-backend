module Wizard.Api.Resource.Project.Version.ProjectVersionChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.Version.ProjectVersionChangeDTO
import Wizard.Api.Resource.Project.Version.ProjectVersionChangeJM ()
import Wizard.Database.Migration.Development.Project.Data.ProjectVersions
import Wizard.Database.Migration.Development.Project.Data.Projects

instance ToSchema ProjectVersionChangeDTO where
  declareNamedSchema = toSwagger (projectVersion2ChangeDto project1Uuid)
