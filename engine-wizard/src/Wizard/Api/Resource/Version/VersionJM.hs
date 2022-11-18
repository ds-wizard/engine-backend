module Wizard.Api.Resource.Version.VersionJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.Version.VersionDTO

instance FromJSON VersionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON VersionDTO where
  toJSON = genericToJSON jsonOptions
