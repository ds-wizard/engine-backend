module Wizard.Api.Resource.UserToken.UserTokenListSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Database.Migration.Development.User.Data.UserTokens
import WizardLib.Public.Api.Resource.UserToken.UserTokenListJM ()
import WizardLib.Public.Model.User.UserTokenList
import WizardLib.Public.Service.UserToken.UserTokenMapper

instance ToSchema UserTokenList where
  declareNamedSchema = toSwagger (toList albertToken True)
