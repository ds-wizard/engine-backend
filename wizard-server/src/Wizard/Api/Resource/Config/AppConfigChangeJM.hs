module Wizard.Api.Resource.Config.AppConfigChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.Config.AppConfigChangeDTO
import Wizard.Api.Resource.Config.AppConfigJM ()

instance FromJSON AppConfigChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppConfigChangeDTO where
  toJSON = genericToJSON jsonOptions
