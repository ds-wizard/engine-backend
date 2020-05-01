module Wizard.Api.Resource.User.UserPasswordJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.User.UserPasswordDTO

instance FromJSON UserPasswordDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON UserPasswordDTO where
  toJSON = genericToJSON simpleOptions
