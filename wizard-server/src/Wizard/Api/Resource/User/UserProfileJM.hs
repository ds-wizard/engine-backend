module Wizard.Api.Resource.User.UserProfileJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Model.User.UserProfile

instance FromJSON UserProfile where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserProfile where
  toJSON = genericToJSON jsonOptions
