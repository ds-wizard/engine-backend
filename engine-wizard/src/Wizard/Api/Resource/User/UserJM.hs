module Wizard.Api.Resource.User.UserJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.User.UserDTO

instance FromJSON UserDTO where
  parseJSON = simpleParseJSON "_userDTO"

instance ToJSON UserDTO where
  toJSON = simpleToJSON "_userDTO"
