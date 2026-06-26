module WizardLib.Public.Model.User.Role where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data Role = Role
  { uuid :: U.UUID
  , name :: String
  , permissions :: [String]
  , isAdmin :: Bool
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance Eq Role where
  a == b =
    a.uuid == b.uuid
      && a.name == b.name
      && a.permissions == b.permissions
      && a.isAdmin == b.isAdmin
      && a.tenantUuid == b.tenantUuid
