module Wizard.Api.Resource.App.AppChangeJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.App.AppChangeDTO

instance FromJSON AppChangeDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON AppChangeDTO where
  toJSON = genericToJSON simpleOptions
