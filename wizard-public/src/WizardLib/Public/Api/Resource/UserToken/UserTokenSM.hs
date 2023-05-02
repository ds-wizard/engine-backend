module WizardLib.Public.Api.Resource.UserToken.UserTokenSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.Public.Api.Resource.UserToken.UserTokenDTO
import WizardLib.Public.Api.Resource.UserToken.UserTokenJM ()

instance ToSchema UserTokenDTO where
  declareNamedSchema = toSwaggerWithFlatType "type" (UserTokenDTO {token = "sometToken"})
