module Shared.Api.Resource.PackageBundle.PackageBundleJM where

import Data.Aeson

import Shared.Api.Resource.Package.PackageJM ()
import Shared.Api.Resource.PackageBundle.PackageBundleDTO
import Shared.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON PackageBundleDTO where
  parseJSON = simpleParseJSON "_packageBundleDTO"

instance ToJSON PackageBundleDTO where
  toJSON = simpleToJSON "_packageBundleDTO"
