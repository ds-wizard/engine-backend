module Api.Resource.User.UserCreateJM where

import Data.Aeson

import Api.Resource.User.UserCreateDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON UserCreateDTO where
  parseJSON = simpleParseJSON "_userCreateDTO"

instance ToJSON UserCreateDTO where
  toJSON = simpleToJSON "_userCreateDTO"
