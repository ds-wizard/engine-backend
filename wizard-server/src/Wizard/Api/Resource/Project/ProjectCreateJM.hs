module Wizard.Api.Resource.Project.ProjectCreateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Project.ProjectCreateDTO
import Wizard.Api.Resource.Project.ProjectSharingJM ()
import Wizard.Api.Resource.Project.ProjectVisibilityJM ()

instance FromJSON ProjectCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectCreateDTO where
  toJSON = genericToJSON jsonOptions
