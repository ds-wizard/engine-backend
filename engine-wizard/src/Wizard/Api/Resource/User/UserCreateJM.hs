module Wizard.Api.Resource.User.UserCreateJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.User.UserCreateDTO

instance FromJSON UserCreateDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON UserCreateDTO where
  toJSON = genericToJSON simpleOptions
