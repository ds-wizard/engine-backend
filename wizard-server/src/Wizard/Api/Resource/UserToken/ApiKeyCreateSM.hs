module Wizard.Api.Resource.UserToken.ApiKeyCreateSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Database.Migration.Development.User.Data.UserTokens
import WizardLib.Public.Api.Resource.UserToken.ApiKeyCreateDTO
import WizardLib.Public.Api.Resource.UserToken.ApiKeyCreateJM ()

instance ToSchema ApiKeyCreateDTO where
  declareNamedSchema = toSwagger albertCreateApiKey
