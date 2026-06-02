module WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientSimpleSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleSM ()
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientSimpleJM ()
import WizardLib.Public.Database.Migration.Development.OpenId.Data.OpenIdClients
import WizardLib.Public.Model.OpenId.OpenIdClientSimple

instance ToSchema OpenIdClientSimple where
  declareNamedSchema = toSwagger defaultOpenIdClientSimple
