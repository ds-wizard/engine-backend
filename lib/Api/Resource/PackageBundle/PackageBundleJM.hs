module Api.Resource.PackageBundle.PackageBundleJM where

import Data.Aeson

import Api.Resource.Package.PackageJM ()
import Api.Resource.PackageBundle.PackageBundleDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON PackageBundleDTO where
  parseJSON = simpleParseJSON "_packageBundleDTO"

instance ToJSON PackageBundleDTO where
  toJSON = simpleToJSON "_packageBundleDTO"
