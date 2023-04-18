module Wizard.Api.Resource.App.AppChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.App.AppChangeDTO

instance FromJSON AppChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppChangeDTO where
  toJSON = genericToJSON jsonOptions
