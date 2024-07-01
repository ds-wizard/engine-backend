module Wizard.Api.Resource.User.UserJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.User.User

instance ToJSON User where
  toJSON = genericToJSON jsonOptions

instance FromJSON UserDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserDTO where
  toJSON = genericToJSON jsonOptions
