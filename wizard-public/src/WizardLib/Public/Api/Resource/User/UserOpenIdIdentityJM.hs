module WizardLib.Public.Api.Resource.User.UserOpenIdIdentityJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleJM ()
import WizardLib.Public.Api.Resource.User.UserOpenIdIdentityDTO

instance FromJSON UserOpenIdIdentityDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON UserOpenIdIdentityDTO where
  toJSON = genericToJSON jsonOptions
