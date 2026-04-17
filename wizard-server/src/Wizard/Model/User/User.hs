module Wizard.Model.User.User where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

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
  , uRole :: String
  , permissions :: [String]
  , active :: Bool
  , imageUrl :: Maybe String
  , locale :: Maybe U.UUID
  , machine :: Bool
  , lastSeenNewsId :: Maybe String
  , tenantUuid :: U.UUID
  , lastVisitedAt :: UTCTime
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , emailVerifiedAt :: Maybe UTCTime
  , emailPending :: Maybe String
  }
  deriving (Generic, Show)

instance Eq User where
  a == b =
    a.uuid == b.uuid
      && a.firstName == b.firstName
      && a.lastName == b.lastName
      && a.email == b.email
      && a.passwordHash == b.passwordHash
      && a.affiliation == b.affiliation
      && a.uRole == b.uRole
      && a.permissions == b.permissions
      && a.active == b.active
      && a.imageUrl == b.imageUrl
      && a.locale == b.locale
      && a.machine == b.machine
      && a.lastSeenNewsId == b.lastSeenNewsId
      && a.tenantUuid == b.tenantUuid
      && a.emailVerifiedAt == b.emailVerifiedAt
      && a.emailPending == b.emailPending
