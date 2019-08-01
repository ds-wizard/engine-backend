module Api.Resource.User.UserChangeJM where

import Data.Aeson

import Api.Resource.User.UserChangeDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON UserChangeDTO where
  parseJSON = simpleParseJSON "_userChangeDTO"

instance ToJSON UserChangeDTO where
  toJSON = simpleToJSON "_userChangeDTO"
