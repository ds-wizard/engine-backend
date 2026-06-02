module WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientParameterSM ()
import Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleSM ()
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailDTO
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailJM ()
import WizardLib.Public.Database.Migration.Development.OpenId.Data.OpenIdClients

instance ToSchema OpenIdClientDetailDTO where
  declareNamedSchema = toSwagger defaultOpenIdClientDetailDto
