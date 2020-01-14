module Wizard.Api.Resource.User.UserJM where

import Data.Aeson

import Wizard.Api.Resource.User.UserDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON UserDTO where
  parseJSON = simpleParseJSON "_userDTO"

instance ToJSON UserDTO where
  toJSON = simpleToJSON "_userDTO"
