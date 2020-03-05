module Wizard.Model.Config.BuildInfoConfigJM where

import Data.Aeson

import Shared.Model.Config.EnvironmentJM ()
import Shared.Util.JSON (simpleParseJSON)
import Wizard.Model.Config.BuildInfoConfig

instance FromJSON BuildInfoConfig where
  parseJSON = simpleParseJSON "_buildInfoConfig"
