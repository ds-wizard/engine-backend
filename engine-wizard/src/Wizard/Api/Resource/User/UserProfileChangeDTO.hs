module Wizard.Api.Resource.User.UserProfileChangeDTO where

import GHC.Generics

import Wizard.Model.User.User

data UserProfileChangeDTO =
  UserProfileChangeDTO
    { _userProfileChangeDTOName :: String
    , _userProfileChangeDTOSurname :: String
    , _userProfileChangeDTOEmail :: Email
    }
  deriving (Generic)
