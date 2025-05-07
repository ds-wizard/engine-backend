module Shared.Locale.Api.Resource.Locale.LocaleSuggestionJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.Locale.Model.Locale.LocaleSuggestion

instance FromJSON LocaleSuggestion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LocaleSuggestion where
  toJSON = genericToJSON jsonOptions
