module Api.Resource.Version.VersionJM where

import Data.Aeson

import Api.Resource.Version.VersionDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON VersionDTO where
  parseJSON = simpleParseJSON "_versionDTO"

instance ToJSON VersionDTO where
  toJSON = simpleToJSON "_versionDTO"
