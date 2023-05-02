module WizardLib.Public.Api.Resource.UserToken.LoginJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.Public.Api.Resource.UserToken.LoginDTO

instance FromJSON LoginDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON LoginDTO where
  toJSON = genericToJSON jsonOptions
