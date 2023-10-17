module WizardLib.Public.Model.User.UserGroup where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data UserGroup = UserGroup
  { uuid :: U.UUID
  , name :: String
  , description :: Maybe String
  , private :: Bool
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Eq, Show)
