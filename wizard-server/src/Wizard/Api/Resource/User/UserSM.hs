module Wizard.Api.Resource.User.UserSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserJM ()
import Wizard.Api.Resource.User.UserSubmissionPropSM ()
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Service.User.UserMapper

instance ToSchema UserDTO where
  declareNamedSchema = toSwagger (toDTO userAlbert)
