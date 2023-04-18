module Wizard.Api.Resource.User.UserPasswordSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.User.UserPasswordDTO
import Wizard.Api.Resource.User.UserPasswordJM ()
import Wizard.Database.Migration.Development.User.Data.Users

instance ToSchema UserPasswordDTO where
  declareNamedSchema = toSwagger userPassword
