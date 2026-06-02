module WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientParameterJM ()
import Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleJM ()
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailDTO

instance FromJSON OpenIdClientDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON OpenIdClientDetailDTO where
  toJSON = genericToJSON jsonOptions
