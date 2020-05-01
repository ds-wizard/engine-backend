module Wizard.Api.Resource.Version.VersionJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Version.VersionDTO

instance FromJSON VersionDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON VersionDTO where
  toJSON = genericToJSON simpleOptions
