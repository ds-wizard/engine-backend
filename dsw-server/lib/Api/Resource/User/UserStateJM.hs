module Api.Resource.User.UserStateJM where

import Data.Aeson

import Api.Resource.User.UserStateDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON UserStateDTO where
  parseJSON = simpleParseJSON "_userStateDTO"

instance ToJSON UserStateDTO where
  toJSON = simpleToJSON "_userStateDTO"
