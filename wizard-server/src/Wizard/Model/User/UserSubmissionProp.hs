module Wizard.Model.User.UserSubmissionProp where

import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data UserSubmissionProp = UserSubmissionProp
  { userUuid :: U.UUID
  , serviceId :: String
  , values :: M.Map String String
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic, Show)

instance Eq UserSubmissionProp where
  a == b =
    a.userUuid == b.userUuid
      && a.serviceId == b.serviceId
      && a.values == b.values
      && a.tenantUuid == b.tenantUuid
