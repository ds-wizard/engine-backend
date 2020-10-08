module Shared.Api.Resource.Package.PackageSuggestionJM where

import Data.Aeson

import Shared.Api.Resource.Package.PackageSuggestionDTO
import Shared.Util.JSON

instance FromJSON PackageSuggestionDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON PackageSuggestionDTO where
  toJSON = genericToJSON simpleOptions
