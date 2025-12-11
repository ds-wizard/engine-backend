module Wizard.Api.Resource.Project.Detail.ProjectDetailSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Project.Acl.ProjectPermSM ()
import Wizard.Api.Resource.Project.Detail.ProjectDetailDTO
import Wizard.Api.Resource.Project.Detail.ProjectDetailJM ()
import Wizard.Api.Resource.Project.ProjectSharingSM ()
import Wizard.Api.Resource.Project.ProjectVisibilitySM ()
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Model.Project.Project

instance ToSchema ProjectDetailDTO where
  declareNamedSchema =
    toSwagger $
      ProjectDetailDTO
        { uuid = project1.uuid
        , name = project1.name
        , visibility = project1.visibility
        , sharing = project1.sharing
        , knowledgeModelPackageId = project1.knowledgeModelPackageId
        , isTemplate = project1.isTemplate
        , migrationUuid = Nothing
        , permissions = [project1AlbertEditProjectPermDto]
        , fileCount = 0
        }
