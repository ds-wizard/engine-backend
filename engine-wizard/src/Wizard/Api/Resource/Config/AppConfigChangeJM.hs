module Wizard.Api.Resource.Config.AppConfigChangeJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.Config.AppConfigChangeDTO
import Wizard.Api.Resource.Config.AppConfigJM ()

instance FromJSON AppConfigChangeDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON AppConfigChangeDTO where
  toJSON = genericToJSON simpleOptions
