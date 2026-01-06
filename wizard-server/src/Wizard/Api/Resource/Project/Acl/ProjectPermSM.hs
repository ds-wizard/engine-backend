module Wizard.Api.Resource.Project.Acl.ProjectPermSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Acl.MemberSM ()
import Wizard.Api.Resource.Project.Acl.ProjectPermDTO
import Wizard.Api.Resource.Project.Acl.ProjectPermJM ()
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Project.Acl.ProjectPerm
import Wizard.Service.Project.ProjectMapper

instance ToSchema ProjectPermType

instance ToSchema ProjectPerm where
  declareNamedSchema = toSwagger bioGroupEditProjectPerm

instance ToSchema ProjectPermDTO where
  declareNamedSchema =
    toSwagger (toUserProjectPermDTO bioGroupEditProjectPerm userAlbert)
