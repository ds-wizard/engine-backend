module Registry.Api.Resource.Package.PackageDetailJM where

import Data.Aeson

import Registry.Api.Resource.Package.PackageDetailDTO
import Shared.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON PackageDetailDTO where
  parseJSON = simpleParseJSON "_packageDetailDTO"

instance ToJSON PackageDetailDTO where
  toJSON = simpleToJSON "_packageDetailDTO"
