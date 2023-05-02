module Wizard.Api.Resource.User.UserSuggestionJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.User.UserSuggestionDTO

instance FromJSON UserSuggestionDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserSuggestionDTO where
  toJSON = genericToJSON jsonOptions
