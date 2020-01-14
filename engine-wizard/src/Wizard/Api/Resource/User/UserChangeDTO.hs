module Wizard.Api.Resource.User.UserChangeDTO where

import Data.UUID
import GHC.Generics

import Wizard.Model.User.User

data UserChangeDTO =
  UserChangeDTO
    { _userChangeDTOUuid :: UUID
    , _userChangeDTOName :: String
    , _userChangeDTOSurname :: String
    , _userChangeDTOEmail :: Email
    , _userChangeDTORole :: Role
    , _userChangeDTOActive :: Bool
    }
  deriving (Generic)
