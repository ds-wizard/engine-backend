module Wizard.Api.Resource.UserToken.ApiKeyCreateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Wizard.Api.Resource.UserToken.ApiKeyCreateDTO

instance FromJSON ApiKeyCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ApiKeyCreateDTO where
  toJSON = genericToJSON jsonOptions
