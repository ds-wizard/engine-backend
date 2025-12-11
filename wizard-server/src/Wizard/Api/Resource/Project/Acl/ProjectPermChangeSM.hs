module Wizard.Api.Resource.Project.Acl.ProjectPermChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.Acl.ProjectPermChangeDTO
import Wizard.Api.Resource.Project.Acl.ProjectPermChangeJM ()
import Wizard.Api.Resource.Project.Acl.ProjectPermSM ()
import Wizard.Api.Resource.Project.ProjectSharingSM ()
import Wizard.Api.Resource.Project.ProjectVisibilitySM ()
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Service.Project.ProjectMapper

instance ToSchema ProjectPermChangeDTO where
  declareNamedSchema = toSwagger (toProjectPermChangeDTO project1AlbertEditProjectPerm)
