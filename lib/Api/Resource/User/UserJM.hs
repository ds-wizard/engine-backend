module Api.Resource.User.UserJM where

import Data.Aeson

import Api.Resource.User.UserDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON UserDTO where
  parseJSON = simpleParseJSON "_userDTO"

instance ToJSON UserDTO where
  toJSON = simpleToJSON "_userDTO"
