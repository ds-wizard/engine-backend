module Wizard.Api.Resource.Project.Detail.ProjectDetailWsJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import Wizard.Api.Resource.Project.Acl.ProjectPermJM ()
import Wizard.Api.Resource.Project.Detail.ProjectDetailWsDTO
import Wizard.Api.Resource.Project.ProjectSharingJM ()
import Wizard.Api.Resource.Project.ProjectVisibilityJM ()

instance FromJSON ProjectDetailWsDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectDetailWsDTO where
  toJSON = genericToJSON jsonOptions
