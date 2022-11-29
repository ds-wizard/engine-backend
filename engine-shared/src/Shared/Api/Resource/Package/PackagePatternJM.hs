module Shared.Api.Resource.Package.PackagePatternJM where

import Data.Aeson

import Shared.Model.Package.PackagePattern
import Shared.Util.Aeson

instance FromJSON PackagePattern where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackagePattern where
  toJSON = genericToJSON jsonOptions
