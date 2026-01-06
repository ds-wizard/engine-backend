module Wizard.Api.Resource.Project.ProjectCreateFromTemplateSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.ProjectCreateFromTemplateDTO
import Wizard.Api.Resource.Project.ProjectCreateFromTemplateJM ()
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Service.Project.ProjectMapper

instance ToSchema ProjectCreateFromTemplateDTO where
  declareNamedSchema = toSwagger (toCreateFromTemplateDTO project1)
