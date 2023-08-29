module Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleJM ()
import Shared.OpenId.Database.Migration.Development.OpenId.Data.OpenIds
import Shared.OpenId.Model.OpenId.OpenIdClientStyle

instance ToSchema OpenIdClientStyle where
  declareNamedSchema = toSwagger openIdClientDefinitionStyle
