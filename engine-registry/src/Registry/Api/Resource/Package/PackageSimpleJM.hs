module Registry.Api.Resource.Package.PackageSimpleJM where

import Data.Aeson

import Registry.Api.Resource.Package.PackageSimpleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Util.Aeson

instance FromJSON PackageSimpleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackageSimpleDTO where
  toJSON = genericToJSON jsonOptions
