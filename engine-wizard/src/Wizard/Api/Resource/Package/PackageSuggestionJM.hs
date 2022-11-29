module Wizard.Api.Resource.Package.PackageSuggestionJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Model.Package.PackageSuggestion

instance FromJSON PackageSuggestion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON PackageSuggestion where
  toJSON = genericToJSON jsonOptions
