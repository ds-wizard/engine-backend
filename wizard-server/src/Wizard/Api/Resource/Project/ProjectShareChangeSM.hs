module Wizard.Api.Resource.Project.ProjectShareChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.Acl.ProjectPermChangeSM ()
import Wizard.Api.Resource.Project.ProjectShareChangeDTO
import Wizard.Api.Resource.Project.ProjectShareChangeJM ()
import Wizard.Api.Resource.Project.ProjectSharingSM ()
import Wizard.Api.Resource.Project.ProjectVisibilitySM ()
import Wizard.Database.Migration.Development.Project.Data.Projects

instance ToSchema ProjectShareChangeDTO where
  declareNamedSchema = toSwagger project1EditedShareChange
