module Registry.Api.Resource.Package.PackageSimpleJM where

import Data.Aeson

import Registry.Api.Resource.Package.PackageSimpleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON PackageSimpleDTO where
  parseJSON = simpleParseJSON "_packageSimpleDTO"

instance ToJSON PackageSimpleDTO where
  toJSON = simpleToJSON "_packageSimpleDTO"
