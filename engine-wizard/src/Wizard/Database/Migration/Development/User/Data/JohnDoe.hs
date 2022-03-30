module Wizard.Database.Migration.Development.User.Data.JohnDoe where

import Wizard.Api.Resource.User.UserCreateDTO
import Wizard.Model.User.User

userJohnCreate :: UserCreateDTO
userJohnCreate =
  UserCreateDTO
    { _userCreateDTOFirstName = "John"
    , _userCreateDTOLastName = "Doe"
    , _userCreateDTOEmail = "john.doe@example.com"
    , _userCreateDTOAffiliation = Just "My University"
    , _userCreateDTORole = Just _USER_ROLE_ADMIN
    , _userCreateDTOPassword = "password"
    }

userJohnCreateDS :: UserCreateDTO
userJohnCreateDS = userJohnCreate {_userCreateDTORole = Just _USER_ROLE_DATA_STEWARD}
