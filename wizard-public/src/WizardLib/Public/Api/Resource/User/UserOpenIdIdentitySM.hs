module WizardLib.Public.Api.Resource.User.UserOpenIdIdentitySM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.OpenId.Api.Resource.OpenId.Client.Definition.OpenIdClientStyleSM ()
import WizardLib.Public.Api.Resource.User.UserOpenIdIdentityDTO
import WizardLib.Public.Api.Resource.User.UserOpenIdIdentityJM ()
import WizardLib.Public.Database.Migration.Development.User.Data.UserOpenIdIdentities

instance ToSchema UserOpenIdIdentityDTO where
  declareNamedSchema = toSwagger defaultUserOpenIdIdentityDto
