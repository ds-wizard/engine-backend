module Wizard.Model.User.User where

import qualified Data.Map.Strict as M
import Data.Time
import Data.UUID
import GHC.Generics

type Permission = String

type Role = String

type Email = String

_USER_SOURCE_INTERNAL = "internal"

_USER_ROLE_ADMIN = "admin"

_USER_ROLE_DATA_STEWARD = "dataSteward"

_USER_ROLE_RESEARCHER = "researcher"

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
    , _userSubmissionProps :: [UserSubmissionProps]
    , _userCreatedAt :: Maybe UTCTime
    , _userUpdatedAt :: Maybe UTCTime
    }
  deriving (Generic, Show)

data UserSubmissionProps =
  UserSubmissionProps
    { _userSubmissionPropsSId :: String
    , _userSubmissionPropsValues :: M.Map String String
    }
  deriving (Generic, Eq, Show)

instance Eq User where
  a == b =
    _userUuid a == _userUuid b &&
    _userFirstName a == _userFirstName b &&
    _userLastName a == _userLastName b &&
    _userEmail a == _userEmail b &&
    _userPasswordHash a == _userPasswordHash b &&
    _userAffiliation a == _userAffiliation b &&
    _userSources a == _userSources b &&
    _userRole a == _userRole b &&
    _userPermissions a == _userPermissions b &&
    _userActive a == _userActive b && _userSubmissionProps a == _userSubmissionProps b
