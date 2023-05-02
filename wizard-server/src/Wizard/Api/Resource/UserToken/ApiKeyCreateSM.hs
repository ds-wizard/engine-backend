module Wizard.Api.Resource.UserToken.ApiKeyCreateSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.UserToken.ApiKeyCreateDTO
import Wizard.Api.Resource.UserToken.ApiKeyCreateJM ()
import Wizard.Database.Migration.Development.User.Data.UserTokens

instance ToSchema ApiKeyCreateDTO where
  declareNamedSchema = toSwagger albertCreateApiKey
