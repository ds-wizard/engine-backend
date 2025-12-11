module Wizard.Api.Resource.Project.ProjectCreateFromTemplateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Project.ProjectCreateFromTemplateDTO

instance FromJSON ProjectCreateFromTemplateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectCreateFromTemplateDTO where
  toJSON = genericToJSON jsonOptions
