module Api.Resource.Package.PackageJM where

import Data.Aeson

import Api.Resource.Event.EventJM ()
import Api.Resource.Package.PackageDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON PackageDTO where
  parseJSON = simpleParseJSON "_packageDTO"

instance ToJSON PackageDTO where
  toJSON = simpleToJSON "_packageDTO"
