module Wizard.Api.Resource.UserToken.LoginSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.UserToken.LoginDTO
import Wizard.Api.Resource.UserToken.LoginJM ()
import Wizard.Database.Migration.Development.User.Data.UserTokens

instance ToSchema LoginDTO where
  declareNamedSchema = toSwagger albertCreateToken
