module Wizard.Api.Resource.User.Group.UserGroupSuggestionJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.User.UserGroupSuggestion

instance FromJSON UserGroupSuggestion where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserGroupSuggestion where
  toJSON = genericToJSON jsonOptions
