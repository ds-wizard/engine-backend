module Wizard.Api.Resource.UserToken.UserTokenListSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.UserToken.UserTokenListJM ()
import Wizard.Database.Migration.Development.User.Data.UserTokens
import Wizard.Model.User.UserTokenList
import Wizard.Service.UserToken.UserTokenMapper

instance ToSchema UserTokenList where
  declareNamedSchema = toSwagger (toList albertToken True)
