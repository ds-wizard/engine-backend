module WizardLib.Public.Api.Resource.UserToken.AppKeyCreateJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.Public.Api.Resource.UserToken.AppKeyCreateDTO

instance FromJSON AppKeyCreateDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON AppKeyCreateDTO where
  toJSON = genericToJSON jsonOptions
