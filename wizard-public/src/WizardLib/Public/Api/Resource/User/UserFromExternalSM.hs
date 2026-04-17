module WizardLib.Public.Api.Resource.User.UserFromExternalSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import WizardLib.Public.Api.Resource.User.UserFromExternalDTO
import WizardLib.Public.Api.Resource.User.UserFromExternalJM ()

instance ToSchema UserFromExternalDTO where
  declareNamedSchema =
    toSwagger
      UserFromExternalDTO
        { hash = "00000000-0000-0000-0000-000000000000"
        , email = "albert.einstein@example.com"
        , firstName = "Albert"
        , lastName = "Einstein"
        }
