module Wizard.Api.Resource.Project.Version.ProjectVersionRevertJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Project.Version.ProjectVersionRevertDTO

instance FromJSON ProjectVersionRevertDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectVersionRevertDTO where
  toJSON = genericToJSON jsonOptions
