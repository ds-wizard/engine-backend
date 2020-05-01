module Shared.Model.Config.BuildInfoConfigJM where

import Data.Aeson

import Shared.Model.Config.BuildInfoConfig
import Shared.Model.Config.EnvironmentJM ()
import Shared.Util.JSON

instance FromJSON BuildInfoConfig where
  parseJSON = simpleParseJSON "_buildInfoConfig"
