module Api.Resource.Event.EventPathJM where

import Data.Aeson

import Api.Resource.Event.EventPathDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON EventPathItemDTO where
  parseJSON = simpleParseJSON "_eventPathItemDTO"

instance ToJSON EventPathItemDTO where
  toJSON = simpleToJSON "_eventPathItemDTO"
