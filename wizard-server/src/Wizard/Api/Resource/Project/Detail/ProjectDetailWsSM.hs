module Wizard.Api.Resource.Project.Detail.ProjectDetailWsSM where

import qualified Data.Map.Strict as M
import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Wizard.Api.Resource.Project.Acl.ProjectPermSM ()
import Wizard.Api.Resource.Project.Detail.ProjectDetailWsDTO
import Wizard.Api.Resource.Project.Detail.ProjectDetailWsJM ()
import Wizard.Api.Resource.Project.ProjectSharingSM ()
import Wizard.Api.Resource.Project.ProjectVisibilitySM ()
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Service.Project.ProjectMapper

instance ToSchema ProjectDetailWsDTO where
  declareNamedSchema = toSwagger (toDetailWsDTO project1 Nothing Nothing [] M.empty M.empty M.empty)
