module Wizard.Api.Resource.User.UserStateJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.User.UserStateDTO

instance FromJSON UserStateDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON UserStateDTO where
  toJSON = genericToJSON simpleOptions
