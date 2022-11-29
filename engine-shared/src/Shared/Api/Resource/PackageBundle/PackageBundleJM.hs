module Shared.Api.Resource.PackageBundle.PackageBundleJM where

import Data.Aeson

import Shared.Api.Resource.Package.PackageJM ()
import Shared.Api.Resource.PackageBundle.PackageBundleDTO
import Shared.Util.Aeson

instance FromJSON PackageBundleDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackageBundleDTO where
  toJSON = genericToJSON jsonOptions
