module Wizard.Api.Resource.Level.LevelJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Level.LevelDTO

instance FromJSON LevelDTO where
  parseJSON = simpleParseJSON "_levelDTO"

instance ToJSON LevelDTO where
  toJSON = simpleToJSON "_levelDTO"
