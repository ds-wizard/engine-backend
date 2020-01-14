module Registry.Api.Resource.Package.PackageJM where

import Data.Aeson

import Registry.Api.Resource.Event.EventJM ()
import Registry.Api.Resource.Package.PackageDTO
import Registry.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON PackageDTO where
  parseJSON = simpleParseJSON "_packageDTO"

instance ToJSON PackageDTO where
  toJSON = simpleToJSON "_packageDTO"
