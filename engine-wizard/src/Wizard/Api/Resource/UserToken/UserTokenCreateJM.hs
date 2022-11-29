module Wizard.Api.Resource.UserToken.UserTokenCreateJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.UserToken.UserTokenCreateDTO

instance FromJSON UserTokenCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserTokenCreateDTO where
  toJSON = genericToJSON jsonOptions
