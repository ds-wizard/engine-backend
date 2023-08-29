module Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.OpenId.Model.OpenId.OpenIdClientStyle

instance FromJSON OpenIdClientStyle where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON OpenIdClientStyle where
  toJSON = genericToJSON jsonOptions
