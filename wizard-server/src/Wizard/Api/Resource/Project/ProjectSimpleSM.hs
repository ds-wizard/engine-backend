module Wizard.Api.Resource.Project.ProjectSimpleSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.ProjectSimpleJM ()
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Model.Project.ProjectSimple
import Wizard.Service.Project.ProjectMapper

instance ToSchema ProjectSimple where
  declareNamedSchema = toSwagger (toSimple project1)
