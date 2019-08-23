module Api.Resource.User.UserPasswordJM where

import Data.Aeson

import Api.Resource.User.UserPasswordDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON UserPasswordDTO where
  parseJSON = simpleParseJSON "_userPasswordDTO"

instance ToJSON UserPasswordDTO where
  toJSON = simpleToJSON "_userPasswordDTO"
