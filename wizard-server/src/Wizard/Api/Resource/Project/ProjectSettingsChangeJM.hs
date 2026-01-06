module Wizard.Api.Resource.Project.ProjectSettingsChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Project.ProjectSettingsChangeDTO

instance FromJSON ProjectSettingsChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ProjectSettingsChangeDTO where
  toJSON = genericToJSON jsonOptions
