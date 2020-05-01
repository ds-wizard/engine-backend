module Registry.Api.Resource.Package.PackageSimpleJM where

import Data.Aeson

import Registry.Api.Resource.Package.PackageSimpleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Util.JSON

instance FromJSON PackageSimpleDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON PackageSimpleDTO where
  toJSON = genericToJSON simpleOptions
