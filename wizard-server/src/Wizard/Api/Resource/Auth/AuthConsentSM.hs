module Wizard.Api.Resource.Auth.AuthConsentSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Auth.AuthConsentDTO
import Wizard.Api.Resource.Auth.AuthConsentJM ()

instance ToSchema AuthConsentDTO where
  declareNamedSchema = toSwagger (AuthConsentDTO {hash = "123", sessionState = Just "456"})
