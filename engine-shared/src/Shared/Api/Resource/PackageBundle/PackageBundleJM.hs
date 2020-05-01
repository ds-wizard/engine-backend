module Shared.Api.Resource.PackageBundle.PackageBundleJM where

import Data.Aeson

import Shared.Api.Resource.Package.PackageJM ()
import Shared.Api.Resource.PackageBundle.PackageBundleDTO
import Shared.Util.JSON

instance FromJSON PackageBundleDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON PackageBundleDTO where
  toJSON = genericToJSON simpleOptions
