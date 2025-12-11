module Wizard.Api.Resource.Project.Action.ProjectActionChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.Action.ProjectActionChangeDTO
import Wizard.Api.Resource.Project.Action.ProjectActionChangeJM ()
import Wizard.Database.Migration.Development.Project.Data.ProjectActions
import Wizard.Service.Project.Action.ProjectActionMapper

instance ToSchema ProjectActionChangeDTO where
  declareNamedSchema = toSwagger (toChangeDTO projectActionFtp1)
