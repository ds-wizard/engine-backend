module Wizard.Api.Resource.Version.VersionJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Version.VersionDTO

instance FromJSON VersionDTO where
  parseJSON = simpleParseJSON "_versionDTO"

instance ToJSON VersionDTO where
  toJSON = simpleToJSON "_versionDTO"
