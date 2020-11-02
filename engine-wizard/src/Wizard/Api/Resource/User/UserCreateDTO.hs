module Wizard.Api.Resource.User.UserCreateDTO where

import GHC.Generics

data UserCreateDTO =
  UserCreateDTO
    { _userCreateDTOFirstName :: String
    , _userCreateDTOLastName :: String
    , _userCreateDTOEmail :: String
    , _userCreateDTOAffiliation :: Maybe String
    , _userCreateDTORole :: Maybe String
    , _userCreateDTOPassword :: String
    }
  deriving (Generic)
