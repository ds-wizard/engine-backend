module Wizard.Model.User.User where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Model.Acl.Acl

_USER_SOURCE_INTERNAL = "internal"

_USER_ROLE_ADMIN = "admin"

_USER_ROLE_DATA_STEWARD = "dataSteward"

_USER_ROLE_RESEARCHER = "researcher"

data User =
  User
    { _userUuid :: U.UUID
    , _userFirstName :: String
    , _userLastName :: String
    , _userEmail :: String
    , _userPasswordHash :: String
    , _userAffiliation :: Maybe String
    , _userSources :: [String]
    , _userRole :: String
    , _userPermissions :: [String]
    , _userActive :: Bool
    , _userSubmissionProps :: [UserSubmissionProps]
    , _userImageUrl :: Maybe String
    , _userGroups :: [GroupMembership]
    , _userAppUuid :: U.UUID
    , _userLastVisitedAt :: UTCTime
    , _userCreatedAt :: UTCTime
    , _userUpdatedAt :: UTCTime
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
    _userActive a == _userActive b &&
    _userSubmissionProps a == _userSubmissionProps b &&
    _userImageUrl a == _userImageUrl b && _userGroups a == _userGroups b && _userAppUuid a == _userAppUuid b
