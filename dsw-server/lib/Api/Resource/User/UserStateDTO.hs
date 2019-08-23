module Api.Resource.User.UserStateDTO where

import GHC.Generics

data UserStateDTO = UserStateDTO
  { _userStateDTOActive :: Bool
  } deriving (Generic)
