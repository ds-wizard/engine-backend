module Wizard.Api.Resource.Package.PackageSuggestionJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Model.Package.PackageSuggestion

instance FromJSON PackageSuggestion where
  parseJSON = simpleParseJSON "_packageSuggestion"

instance ToJSON PackageSuggestion where
  toJSON = simpleToJSON "_packageSuggestion"
