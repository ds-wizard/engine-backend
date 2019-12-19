module Wizard.Api.Resource.PackageBundle.PackageBundleJM where

import Data.Aeson

import Wizard.Api.Resource.Package.PackageJM ()
import Wizard.Api.Resource.PackageBundle.PackageBundleDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON PackageBundleDTO where
  parseJSON = simpleParseJSON "_packageBundleDTO"

instance ToJSON PackageBundleDTO where
  toJSON = simpleToJSON "_packageBundleDTO"
