module Wizard.Api.Resource.App.AppJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.App.AppDTO

instance FromJSON AppDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON AppDTO where
  toJSON = genericToJSON simpleOptions
