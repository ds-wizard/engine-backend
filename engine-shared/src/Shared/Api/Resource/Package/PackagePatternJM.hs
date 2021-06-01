module Shared.Api.Resource.Package.PackagePatternJM where

import Data.Aeson

import Shared.Model.Package.PackagePattern
import Shared.Util.JSON

instance FromJSON PackagePattern where
  parseJSON = simpleParseJSON "_packagePattern"

instance ToJSON PackagePattern where
  toJSON = simpleToJSON "_packagePattern"
