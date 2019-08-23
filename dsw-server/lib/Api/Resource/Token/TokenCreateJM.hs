module Api.Resource.Token.TokenCreateJM where

import Data.Aeson

import Api.Resource.Token.TokenCreateDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON TokenCreateDTO where
  parseJSON = simpleParseJSON "_tokenCreateDTO"

instance ToJSON TokenCreateDTO where
  toJSON = simpleToJSON "_tokenCreateDTO"
