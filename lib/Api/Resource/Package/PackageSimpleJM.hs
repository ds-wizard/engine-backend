module Api.Resource.Package.PackageSimpleJM where

import Data.Aeson

import Api.Resource.Organization.OrganizationSimpleJM ()
import Api.Resource.Package.PackageSimpleDTO
import Api.Resource.Package.PackageStateJM ()
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON PackageSimpleDTO where
  parseJSON = simpleParseJSON "_packageSimpleDTO"

instance ToJSON PackageSimpleDTO where
  toJSON = simpleToJSON "_packageSimpleDTO"
