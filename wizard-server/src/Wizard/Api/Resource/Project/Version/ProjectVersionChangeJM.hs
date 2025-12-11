module Wizard.Api.Resource.Project.Version.ProjectVersionChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Project.Version.ProjectVersionChangeDTO

instance FromJSON ProjectVersionChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectVersionChangeDTO where
  toJSON = genericToJSON jsonOptions
