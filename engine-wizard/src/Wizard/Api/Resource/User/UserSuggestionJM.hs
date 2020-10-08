module Wizard.Api.Resource.User.UserSuggestionJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.User.UserSuggestionDTO

instance FromJSON UserSuggestionDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON UserSuggestionDTO where
  toJSON = genericToJSON simpleOptions
