module Wizard.Api.Resource.UserToken.UserTokenCreateSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.UserToken.UserTokenCreateDTO
import Wizard.Api.Resource.UserToken.UserTokenCreateJM ()
import Wizard.Database.Migration.Development.User.Data.UserTokens

instance ToSchema UserTokenCreateDTO where
  declareNamedSchema = simpleToSchema albertCreateToken
