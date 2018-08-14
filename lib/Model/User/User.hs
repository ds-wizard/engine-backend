module Model.User.User where

import Data.Time
import Data.UUID
import GHC.Generics

type Permission = String

type Role = String

type Email = String

data User = User
  { _userUuid :: UUID
  , _userName :: String
  , _userSurname :: String
  , _userEmail :: Email
  , _userPasswordHash :: String
  , _userRole :: Role
  , _userPermissions :: [Permission]
  , _userIsActive :: Bool
  , _userCreatedAt :: Maybe UTCTime
  , _userUpdatedAt :: Maybe UTCTime
  } deriving (Generic, Show)

instance Eq User where
  a == b =
    _userUuid a == _userUuid b &&
    _userName a == _userName b &&
    _userSurname a == _userSurname b &&
    _userEmail a == _userEmail b &&
    _userPasswordHash a == _userPasswordHash b &&
    _userRole a == _userRole b && _userPermissions a == _userPermissions b && _userIsActive a == _userIsActive b
