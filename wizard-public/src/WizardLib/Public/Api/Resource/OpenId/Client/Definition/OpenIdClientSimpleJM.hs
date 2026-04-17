module WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientSimpleJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleJM ()
import WizardLib.Public.Model.OpenId.OpenIdClientSimple

instance FromJSON OpenIdClientSimple where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON OpenIdClientSimple where
  toJSON = genericToJSON jsonOptions
