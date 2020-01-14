module Wizard.Api.Resource.Level.LevelJM where

import Data.Aeson

import Wizard.Api.Resource.Level.LevelDTO
import Wizard.Util.JSON (simpleParseJSON, simpleToJSON)

instance FromJSON LevelDTO where
  parseJSON = simpleParseJSON "_levelDTO"

instance ToJSON LevelDTO where
  toJSON = simpleToJSON "_levelDTO"
