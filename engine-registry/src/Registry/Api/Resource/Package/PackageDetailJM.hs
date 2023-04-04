module Registry.Api.Resource.Package.PackageDetailJM where

import Data.Aeson

import Registry.Api.Resource.Package.PackageDetailDTO
import Shared.Api.Resource.Organization.OrganizationSimpleJM ()
import Shared.Api.Resource.Package.PackagePhaseJM ()
import Shared.Util.Aeson

instance FromJSON PackageDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackageDetailDTO where
  toJSON = genericToJSON jsonOptions
