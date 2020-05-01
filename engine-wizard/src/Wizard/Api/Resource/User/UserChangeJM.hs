module Wizard.Api.Resource.User.UserChangeJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.User.UserChangeDTO

instance FromJSON UserChangeDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON UserChangeDTO where
  toJSON = genericToJSON simpleOptions
