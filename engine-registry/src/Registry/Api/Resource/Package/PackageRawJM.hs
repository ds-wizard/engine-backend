module Registry.Api.Resource.Package.PackageRawJM where

import Data.Aeson

import Registry.Api.Resource.Package.PackageRawDTO
import Shared.Api.Resource.Package.PackagePhaseJM ()
import Shared.Util.Aeson

instance ToJSON PackageRawDTO where
  toJSON = genericToJSON jsonOptions

instance FromJSON PackageRawDTO where
  parseJSON = genericParseJSON jsonOptions
