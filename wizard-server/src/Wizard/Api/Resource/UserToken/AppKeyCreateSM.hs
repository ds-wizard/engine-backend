module Wizard.Api.Resource.UserToken.AppKeyCreateSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Database.Migration.Development.User.Data.UserTokens
import WizardLib.Public.Api.Resource.UserToken.AppKeyCreateDTO
import WizardLib.Public.Api.Resource.UserToken.AppKeyCreateJM ()

instance ToSchema AppKeyCreateDTO where
  declareNamedSchema = toSwagger myAppKeyCreate
