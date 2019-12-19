module Wizard.Model.Config.BuildInfoConfigJM where

import Data.Aeson

import Shared.Model.Config.EnvironmentJM ()
import Wizard.Model.Config.BuildInfoConfig
import Wizard.Util.JSON (simpleParseJSON)

instance FromJSON BuildInfoConfig where
  parseJSON = simpleParseJSON "_buildInfoConfig"
