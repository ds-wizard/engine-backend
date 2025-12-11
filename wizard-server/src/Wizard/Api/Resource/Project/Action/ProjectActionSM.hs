module Wizard.Api.Resource.Project.Action.ProjectActionSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.Action.ProjectActionDTO
import Wizard.Api.Resource.Project.Action.ProjectActionJM ()
import Wizard.Database.Migration.Development.Project.Data.ProjectActions
import Wizard.Service.Project.Action.ProjectActionMapper

instance ToSchema ProjectActionDTO where
  declareNamedSchema = toSwagger (toDTO projectActionFtp1)
