module Wizard.Api.Resource.App.AppJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.App.AppDTO
import Wizard.Model.App.App

instance FromJSON App where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON App where
  toJSON = genericToJSON jsonOptions

instance FromJSON AppDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppDTO where
  toJSON = genericToJSON jsonOptions
