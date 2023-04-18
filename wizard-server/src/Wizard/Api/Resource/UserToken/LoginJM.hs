module Wizard.Api.Resource.UserToken.LoginJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.UserToken.LoginDTO

instance FromJSON LoginDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LoginDTO where
  toJSON = genericToJSON jsonOptions
