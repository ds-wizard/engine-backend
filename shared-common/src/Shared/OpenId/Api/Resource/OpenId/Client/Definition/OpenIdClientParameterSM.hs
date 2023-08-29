module Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientParameterSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientParameterJM ()
import Shared.OpenId.Database.Migration.Development.OpenId.Data.OpenIds
import Shared.OpenId.Model.OpenId.OpenIdClientParameter

instance ToSchema OpenIdClientParameter where
  declareNamedSchema = toSwagger openIdClientDefinitionParameter
