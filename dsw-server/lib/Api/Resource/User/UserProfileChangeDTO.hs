module Api.Resource.User.UserProfileChangeDTO where

import GHC.Generics

import Model.User.User

data UserProfileChangeDTO = UserProfileChangeDTO
  { _userProfileChangeDTOName :: String
  , _userProfileChangeDTOSurname :: String
  , _userProfileChangeDTOEmail :: Email
  } deriving (Generic)
