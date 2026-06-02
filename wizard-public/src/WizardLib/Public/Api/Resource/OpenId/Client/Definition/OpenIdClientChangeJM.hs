module WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientParameterJM ()
import Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleJM ()
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeDTO

instance FromJSON OpenIdClientChangeDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON OpenIdClientChangeDTO where
  toJSON = genericToJSON jsonOptions
