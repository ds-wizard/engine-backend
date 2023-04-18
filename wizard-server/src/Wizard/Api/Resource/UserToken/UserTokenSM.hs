module Wizard.Api.Resource.UserToken.UserTokenSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.UserToken.UserTokenDTO
import Wizard.Api.Resource.UserToken.UserTokenJM ()
import Wizard.Database.Migration.Development.User.Data.UserTokens

instance ToSchema UserTokenDTO where
  declareNamedSchema = toSwaggerWithFlatType "type" albertTokenDto
