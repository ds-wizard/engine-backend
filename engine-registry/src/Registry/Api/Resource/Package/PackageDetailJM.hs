module Registry.Api.Resource.Package.PackageDetailJM where

import Data.Aeson

import Registry.Api.Resource.Package.PackageDetailDTO
import Shared.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Util.JSON

instance FromJSON PackageDetailDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON PackageDetailDTO where
  toJSON = genericToJSON simpleOptions
