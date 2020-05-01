module Wizard.Api.Resource.User.UserCreateSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.User.UserCreateDTO
import Wizard.Api.Resource.User.UserCreateJM ()
import Wizard.Database.Migration.Development.User.Data.Users

instance ToSchema UserCreateDTO where
  declareNamedSchema = simpleToSchema userJohnCreate
