module Wizard.Api.Resource.Project.ProjectCreateSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.ProjectCreateDTO
import Wizard.Api.Resource.Project.ProjectCreateJM ()
import Wizard.Api.Resource.Project.ProjectSharingSM ()
import Wizard.Api.Resource.Project.ProjectVisibilitySM ()
import Wizard.Database.Migration.Development.Project.Data.Projects

instance ToSchema ProjectCreateDTO where
  declareNamedSchema = toSwagger project1Create
