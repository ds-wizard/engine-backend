module Wizard.Api.Resource.App.AppCreateJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.App.AppCreateDTO

instance FromJSON AppCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppCreateDTO where
  toJSON = genericToJSON jsonOptions
