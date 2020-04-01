module Wizard.Api.Resource.User.UserCreateDTO where

import GHC.Generics

import Wizard.Model.User.User

data UserCreateDTO =
  UserCreateDTO
    { _userCreateDTOFirstName :: String
    , _userCreateDTOLastName :: String
    , _userCreateDTOEmail :: Email
    , _userCreateDTOAffiliation :: Maybe String
    , _userCreateDTORole :: Maybe Role
    , _userCreateDTOPassword :: String
    }
  deriving (Generic)
