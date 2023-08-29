module Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientParameterJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.OpenId.Model.OpenId.OpenIdClientParameter

instance FromJSON OpenIdClientParameter where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON OpenIdClientParameter where
  toJSON = genericToJSON jsonOptions
