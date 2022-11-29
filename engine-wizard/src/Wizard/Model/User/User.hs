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

data User = User
  { uuid :: U.UUID
  , firstName :: String
  , lastName :: String
  , email :: String
  , passwordHash :: String
  , affiliation :: Maybe String
  , sources :: [String]
  , uRole :: String
  , permissions :: [String]
  , active :: Bool
  , submissionProps :: [UserSubmissionProps]
  , imageUrl :: Maybe String
  , groups :: [GroupMembership]
  , machine :: Bool
  , appUuid :: U.UUID
  , lastVisitedAt :: UTCTime
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

data UserSubmissionProps = UserSubmissionProps
  { sId :: String
  , values :: M.Map String String
  }
  deriving (Generic, Eq, Show)

instance Eq User where
  a == b =
    a.uuid == b.uuid
      && a.firstName == b.firstName
      && a.lastName == b.lastName
      && a.email == b.email
      && a.passwordHash == b.passwordHash
      && a.affiliation == b.affiliation
      && a.sources == b.sources
      && a.uRole == b.uRole
      && a.permissions == b.permissions
      && a.active == b.active
      && a.submissionProps == b.submissionProps
      && a.imageUrl == b.imageUrl
      && a.groups == b.groups
      && a.machine == b.machine
      && a.appUuid == b.appUuid
