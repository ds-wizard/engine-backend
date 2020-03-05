module Registry.Model.Config.BuildInfoConfigJM where

import Data.Aeson

import Registry.Model.Config.BuildInfoConfig
import Shared.Model.Config.EnvironmentJM ()
import Shared.Util.JSON (simpleParseJSON)

instance FromJSON BuildInfoConfig where
  parseJSON = simpleParseJSON "_buildInfoConfig"
