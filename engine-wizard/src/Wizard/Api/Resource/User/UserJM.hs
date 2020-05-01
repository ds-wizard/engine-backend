module Wizard.Api.Resource.User.UserJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.User.UserDTO

instance FromJSON UserDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON UserDTO where
  toJSON = genericToJSON simpleOptions
