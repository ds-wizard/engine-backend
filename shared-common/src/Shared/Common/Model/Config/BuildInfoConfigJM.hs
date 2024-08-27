module Shared.Common.Model.Config.BuildInfoConfigJM where

import Data.Aeson

import Shared.Common.Model.Config.BuildInfoConfig
import Shared.Common.Util.Aeson

instance FromJSON BuildInfoConfig where
  parseJSON = genericParseJSON jsonOptions
