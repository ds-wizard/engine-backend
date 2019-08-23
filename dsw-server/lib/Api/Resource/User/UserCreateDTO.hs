module Api.Resource.User.UserCreateDTO where

import GHC.Generics

import Model.User.User

data UserCreateDTO = UserCreateDTO
  { _userCreateDTOName :: String
  , _userCreateDTOSurname :: String
  , _userCreateDTOEmail :: Email
  , _userCreateDTORole :: Maybe Role
  , _userCreateDTOPassword :: String
  } deriving (Generic)
