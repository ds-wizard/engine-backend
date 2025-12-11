module Wizard.Api.Resource.Project.Version.ProjectVersionRevertSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.Version.ProjectVersionRevertDTO
import Wizard.Api.Resource.Project.Version.ProjectVersionRevertJM ()
import Wizard.Database.Migration.Development.Project.Data.ProjectVersions
import Wizard.Database.Migration.Development.Project.Data.Projects

instance ToSchema ProjectVersionRevertDTO where
  declareNamedSchema = toSwagger (projectVersion1RevertDto project1Uuid)
