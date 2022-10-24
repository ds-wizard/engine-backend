module Wizard.Api.Resource.UserToken.UserTokenJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.UserToken.UserTokenDTO

instance FromJSON UserTokenDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON UserTokenDTO where
  toJSON = genericToJSON simpleOptions
