module Wizard.Api.Resource.User.UserStateSM where

import Data.Swagger

import Shared.Util.Swagger
import Wizard.Api.Resource.User.UserStateDTO
import Wizard.Api.Resource.User.UserStateJM ()
import Wizard.Database.Migration.Development.User.Data.Users

instance ToSchema UserStateDTO where
  declareNamedSchema = simpleToSchema userState
