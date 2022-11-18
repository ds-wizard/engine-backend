module Wizard.Api.Resource.User.UserStateJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.User.UserStateDTO

instance FromJSON UserStateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserStateDTO where
  toJSON = genericToJSON jsonOptions
