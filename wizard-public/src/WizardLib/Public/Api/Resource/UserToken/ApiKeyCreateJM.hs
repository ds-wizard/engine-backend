module WizardLib.Public.Api.Resource.UserToken.ApiKeyCreateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.Public.Api.Resource.UserToken.ApiKeyCreateDTO

instance FromJSON ApiKeyCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ApiKeyCreateDTO where
  toJSON = genericToJSON jsonOptions
