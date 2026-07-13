module Shared.OpenId.Api.Resource.OpenId.Client.Flow.OpenIdClientAuthenticationUrlJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.OpenId.Api.Resource.OpenId.Client.Flow.OpenIdClientAuthenticationUrlDTO

instance FromJSON OpenIdClientAuthenticationUrlDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON OpenIdClientAuthenticationUrlDTO where
  toJSON = genericToJSON jsonOptions
