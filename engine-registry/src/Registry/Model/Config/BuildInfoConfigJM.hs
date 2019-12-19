module Registry.Model.Config.BuildInfoConfigJM where

import Data.Aeson

import Registry.Model.Config.BuildInfoConfig
import Registry.Util.JSON (simpleParseJSON)
import Shared.Model.Config.EnvironmentJM ()

instance FromJSON BuildInfoConfig where
  parseJSON = simpleParseJSON "_buildInfoConfig"
