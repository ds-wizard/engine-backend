module Wizard.Api.Resource.User.UserChangeDTO where

import GHC.Generics

import Wizard.Model.User.User

data UserChangeDTO =
  UserChangeDTO
    { _userChangeDTOFirstName :: String
    , _userChangeDTOLastName :: String
    , _userChangeDTOEmail :: Email
    , _userChangeDTOAffiliation :: Maybe String
    , _userChangeDTORole :: Role
    , _userChangeDTOActive :: Bool
    }
  deriving (Generic)
