module WizardLib.Public.Api.Resource.UserToken.LoginSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.Public.Api.Resource.UserToken.LoginDTO
import WizardLib.Public.Api.Resource.UserToken.LoginJM ()

instance ToSchema LoginDTO where
  declareNamedSchema = toSwagger (LoginDTO {email = "albert.einstein@example.com", password = "password", code = Nothing})
