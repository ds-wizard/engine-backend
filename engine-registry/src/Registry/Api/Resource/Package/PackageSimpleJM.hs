module Registry.Api.Resource.Package.PackageSimpleJM where

import Data.Aeson

import Registry.Api.Resource.Organization.OrganizationSimpleJM ()
import Registry.Api.Resource.Package.PackageSimpleDTO
import Registry.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON PackageSimpleDTO where
  parseJSON = simpleParseJSON "_packageSimpleDTO"

instance ToJSON PackageSimpleDTO where
  toJSON = simpleToJSON "_packageSimpleDTO"
