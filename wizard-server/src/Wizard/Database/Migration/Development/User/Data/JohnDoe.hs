module Wizard.Database.Migration.Development.User.Data.JohnDoe where

import Wizard.Api.Resource.User.UserCreateDTO
import Wizard.Model.User.User

userJohnCreate :: UserCreateDTO
userJohnCreate =
  UserCreateDTO
    { firstName = "John"
    , lastName = "Doe"
    , email = "john.doe@example.com"
    , affiliation = Just "My University"
    , uRole = Just _USER_ROLE_ADMIN
    , password = "password"
    }

userJohnCreateDS :: UserCreateDTO
userJohnCreateDS = userJohnCreate {uRole = Just _USER_ROLE_RESEARCHER}
