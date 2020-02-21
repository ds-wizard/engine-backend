module Registry.Api.Resource.PackageBundle.PackageBundleJM where

import Data.Aeson

import Registry.Api.Resource.Package.PackageJM ()
import Registry.Api.Resource.PackageBundle.PackageBundleDTO
import Shared.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON PackageBundleDTO where
  parseJSON = simpleParseJSON "_packageBundleDTO"

instance ToJSON PackageBundleDTO where
  toJSON = simpleToJSON "_packageBundleDTO"
