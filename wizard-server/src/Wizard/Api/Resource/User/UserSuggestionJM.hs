module Wizard.Api.Resource.User.UserSuggestionJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.User.UserSuggestion

instance FromJSON UserSuggestion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserSuggestion where
  toJSON = genericToJSON jsonOptions
