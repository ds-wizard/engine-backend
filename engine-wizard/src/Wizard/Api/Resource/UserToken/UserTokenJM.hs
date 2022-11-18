module Wizard.Api.Resource.UserToken.UserTokenJM where

import Data.Aeson

import Shared.Util.Aeson
import Wizard.Api.Resource.UserToken.UserTokenDTO

instance FromJSON UserTokenDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserTokenDTO where
  toJSON = genericToJSON jsonOptions
