module Api.Resource.User.UserDTO where

import Data.Time
import Data.UUID
import GHC.Generics

import Model.User.User

data UserDTO = UserDTO
  { _userDTOUuid :: UUID
  , _userDTOName :: String
  , _userDTOSurname :: String
  , _userDTOEmail :: Email
  , _userDTORole :: Role
  , _userDTOPermissions :: [Permission]
  , _userDTOActive :: Bool
  , _userDTOCreatedAt :: Maybe UTCTime
  , _userDTOUpdatedAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)
