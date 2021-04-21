module Registry.Api.Resource.Package.PackageRawJM where

import Data.Aeson

import Registry.Api.Resource.Package.PackageRawDTO
import Shared.Util.JSON

instance ToJSON PackageRawDTO where
  toJSON = genericToJSON simpleOptions

instance FromJSON PackageRawDTO where
  parseJSON = genericParseJSON simpleOptions
