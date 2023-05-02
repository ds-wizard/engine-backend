module Wizard.Api.Resource.UserToken.UserTokenListSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.UserToken.UserTokenListJM ()
import Wizard.Database.Migration.Development.User.Data.UserTokens
import Wizard.Service.UserToken.UserTokenMapper
import WizardLib.Public.Model.User.UserTokenList

instance ToSchema UserTokenList where
  declareNamedSchema = toSwagger (toList albertToken True)
