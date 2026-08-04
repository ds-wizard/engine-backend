module Wizard.Model.User.User (
  User (..),
  module WizardLib.Public.Model.User.RoleSimple,
) where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import WizardLib.Public.Model.User.RoleSimple

data User = User
  { uuid :: U.UUID
  , firstName :: String
  , lastName :: String
  , email :: String
  , passwordHash :: String
  , affiliation :: Maybe String
  , role :: RoleSimple
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
      && a.role == b.role
      && a.active == b.active
      && a.imageUrl == b.imageUrl
      && a.locale == b.locale
      && a.machine == b.machine
      && a.lastSeenNewsId == b.lastSeenNewsId
      && a.tenantUuid == b.tenantUuid
      && a.emailVerifiedAt == b.emailVerifiedAt
      && a.emailPending == b.emailPending
