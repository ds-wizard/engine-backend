module Wizard.Api.Resource.UserToken.UserTokenCreateJM where

import Data.Aeson

import Shared.Util.JSON
import Wizard.Api.Resource.UserToken.UserTokenCreateDTO

instance FromJSON UserTokenCreateDTO where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON UserTokenCreateDTO where
  toJSON = genericToJSON simpleOptions
