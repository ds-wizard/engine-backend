module Registry.Api.Resource.PackageBundle.PackageBundleJM where

import Data.Aeson

import Registry.Api.Resource.Package.PackageRawJM ()
import Registry.Api.Resource.PackageBundle.PackageBundleDTO
import Shared.Util.JSON

instance FromJSON PackageBundleDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON PackageBundleDTO where
  toJSON = genericToJSON simpleOptions
