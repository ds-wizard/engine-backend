module Wizard.Api.Resource.User.UserCreateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.User.UserCreateDTO

instance FromJSON UserCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserCreateDTO where
  toJSON = genericToJSON jsonOptions
