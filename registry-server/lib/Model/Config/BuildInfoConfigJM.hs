module Model.Config.BuildInfoConfigJM where

import Data.Aeson

import Model.Config.BuildInfoConfig
import Model.Config.EnvironmentJM ()
import Util.JSON (simpleParseJSON)

instance FromJSON BuildInfoConfig where
  parseJSON = simpleParseJSON "_buildInfoConfig"
