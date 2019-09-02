module Api.Resource.Level.LevelJM where

import Data.Aeson

import Api.Resource.Level.LevelDTO
import Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON LevelDTO where
  parseJSON = simpleParseJSON "_levelDTO"

instance ToJSON LevelDTO where
  toJSON = simpleToJSON "_levelDTO"
