module Wizard.Api.Resource.Project.Detail.ProjectDetailJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Project.Acl.ProjectPermJM ()
import Wizard.Api.Resource.Project.Detail.ProjectDetailDTO
import Wizard.Api.Resource.Project.ProjectSharingJM ()
import Wizard.Api.Resource.Project.ProjectVisibilityJM ()

instance FromJSON ProjectDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectDetailDTO where
  toJSON = genericToJSON jsonOptions
