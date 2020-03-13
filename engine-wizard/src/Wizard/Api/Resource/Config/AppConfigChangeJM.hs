module Wizard.Api.Resource.Config.AppConfigChangeJM where

import Data.Aeson

import Shared.Util.JSON (simpleParseJSON, simpleToJSON)
import Wizard.Api.Resource.Config.AppConfigChangeDTO
import Wizard.Api.Resource.Config.AppConfigJM ()

instance FromJSON AppConfigChangeDTO where
  parseJSON = simpleParseJSON "_appConfigChangeDTO"

instance ToJSON AppConfigChangeDTO where
  toJSON = simpleToJSON "_appConfigChangeDTO"
