module Api.Resource.ActionKey.ActionKeyJM where

import Data.Aeson

import Api.Resource.ActionKey.ActionKeyDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON ActionKeyDTO where
  parseJSON = simpleParseJSON "_actionKeyDTO"

instance ToJSON ActionKeyDTO where
  toJSON = simpleToJSON "_actionKeyDTO"
