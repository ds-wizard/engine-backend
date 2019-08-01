module Api.Resource.Token.TokenJM where

import Data.Aeson

import Api.Resource.Token.TokenDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON TokenDTO where
  parseJSON = simpleParseJSON "_tokenDTO"

instance ToJSON TokenDTO where
  toJSON = simpleToJSON "_tokenDTO"
