module Registry.Api.Resource.PackageBundle.PackageBundleJM where

import Data.Aeson

import Registry.Api.Resource.Package.PackageRawJM ()
import Registry.Api.Resource.PackageBundle.PackageBundleDTO
import Shared.Common.Util.Aeson

instance FromJSON PackageBundleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackageBundleDTO where
  toJSON = genericToJSON jsonOptions
