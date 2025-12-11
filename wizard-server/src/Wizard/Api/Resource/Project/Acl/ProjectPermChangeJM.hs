module Wizard.Api.Resource.Project.Acl.ProjectPermChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Project.Acl.ProjectPermChangeDTO
import Wizard.Api.Resource.Project.Acl.ProjectPermJM ()
import Wizard.Api.Resource.Project.ProjectSharingJM ()
import Wizard.Api.Resource.Project.ProjectVisibilityJM ()

instance FromJSON ProjectPermChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectPermChangeDTO where
  toJSON = genericToJSON jsonOptions
