module WizardLib.Public.Api.Resource.UserToken.UserTokenClaimsJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import WizardLib.Public.Api.Resource.UserToken.UserTokenClaimsDTO

instance FromJSON UserTokenClaimsDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserTokenClaimsDTO where
  toJSON = genericToJSON jsonOptions
