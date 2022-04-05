module Wizard.Api.Resource.App.AppJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.App.AppDTO
import Wizard.Model.App.App

instance FromJSON App where
  parseJSON = simpleParseJSON "_app"

instance ToJSON App where
  toJSON = simpleToJSON "_app"

instance FromJSON AppDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON AppDTO where
  toJSON = genericToJSON simpleOptions
