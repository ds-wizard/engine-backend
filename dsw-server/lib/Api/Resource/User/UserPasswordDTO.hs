module Api.Resource.User.UserPasswordDTO where

import GHC.Generics

data UserPasswordDTO = UserPasswordDTO
  { _userPasswordDTOPassword :: String
  } deriving (Generic)
