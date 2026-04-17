module WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientParameterSM ()
import Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleSM ()
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeDTO
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeJM ()
import WizardLib.Public.Database.Migration.Development.OpenId.Data.OpenIdClients

instance ToSchema OpenIdClientChangeDTO where
  declareNamedSchema = toSwagger defaultOpenIdClientChangeDto
