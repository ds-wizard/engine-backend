module Wizard.Api.Resource.Level.LevelJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Model.Level.Level

instance FromJSON Level where
  parseJSON = simpleParseJSON "_level"

instance ToJSON Level where
  toJSON = simpleToJSON "_level"
