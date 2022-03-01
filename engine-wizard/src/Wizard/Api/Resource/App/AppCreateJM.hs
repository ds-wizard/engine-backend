module Wizard.Api.Resource.App.AppCreateJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.App.AppCreateDTO

instance FromJSON AppCreateDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON AppCreateDTO where
  toJSON = genericToJSON simpleOptions
