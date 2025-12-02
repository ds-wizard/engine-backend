module WizardLib.Public.Api.Resource.User.UserSuggestionJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.Public.Model.User.UserSuggestion

instance FromJSON UserSuggestion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserSuggestion where
  toJSON = genericToJSON jsonOptions
