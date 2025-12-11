module Wizard.Api.Resource.Project.ProjectShareChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Project.Acl.ProjectPermChangeJM ()
import Wizard.Api.Resource.Project.ProjectShareChangeDTO
import Wizard.Api.Resource.Project.ProjectSharingJM ()
import Wizard.Api.Resource.Project.ProjectVisibilityJM ()

instance FromJSON ProjectShareChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectShareChangeDTO where
  toJSON = genericToJSON jsonOptions
