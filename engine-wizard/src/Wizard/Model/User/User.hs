module Wizard.Model.User.User where

import Data.Time
import Data.UUID
import GHC.Generics

type Permission = String

type Role = String

type Email = String

_USER_SOURCE_INTERNAL = "internal"

data User =
  User
    { _userUuid :: UUID
    , _userFirstName :: String
    , _userLastName :: String
    , _userEmail :: Email
    , _userPasswordHash :: String
    , _userAffiliation :: Maybe String
    , _userSources :: [String]
    , _userRole :: Role
    , _userPermissions :: [Permission]
    , _userActive :: Bool
    , _userCreatedAt :: Maybe UTCTime
    , _userUpdatedAt :: Maybe UTCTime
    }
  deriving (Generic, Show)

instance Eq User where
  a == b =
    _userUuid a == _userUuid b &&
    _userFirstName a == _userFirstName b &&
    _userLastName a == _userLastName b &&
    _userEmail a == _userEmail b &&
    _userPasswordHash a == _userPasswordHash b &&
    _userAffiliation a == _userAffiliation b &&
    _userSources a == _userSources b &&
    _userRole a == _userRole b && _userPermissions a == _userPermissions b && _userActive a == _userActive b
