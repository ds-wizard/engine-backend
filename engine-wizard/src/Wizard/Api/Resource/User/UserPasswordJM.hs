module Wizard.Api.Resource.User.UserPasswordJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.User.UserPasswordDTO

instance FromJSON UserPasswordDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserPasswordDTO where
  toJSON = genericToJSON jsonOptions
