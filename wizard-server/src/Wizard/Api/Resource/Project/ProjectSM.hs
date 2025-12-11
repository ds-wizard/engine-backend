module Wizard.Api.Resource.Project.ProjectSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleSM ()
import Wizard.Api.Resource.Project.Acl.ProjectPermSM ()
import Wizard.Api.Resource.Project.ProjectDTO
import Wizard.Api.Resource.Project.ProjectJM ()
import Wizard.Api.Resource.Project.ProjectReportSM ()
import Wizard.Api.Resource.Project.ProjectSharingSM ()
import Wizard.Api.Resource.Project.ProjectStateSM ()
import Wizard.Api.Resource.Project.ProjectVisibilitySM ()
import Wizard.Api.Resource.User.UserSM ()
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Model.Project.ProjectState
import Wizard.Service.Project.ProjectMapper

instance ToSchema ProjectDTO where
  declareNamedSchema =
    toSwagger (toDTO project1 germanyKmPackage DefaultProjectState [project1AlbertEditProjectPermDto])
