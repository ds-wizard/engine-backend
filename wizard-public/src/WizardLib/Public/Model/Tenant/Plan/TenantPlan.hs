module WizardLib.Public.Model.Tenant.Plan.TenantPlan where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

data TenantPlan = TenantPlan
  { uuid :: U.UUID
  , name :: String
  , users :: Maybe Int
  , since :: Maybe UTCTime
  , until :: Maybe UTCTime
  , test :: Bool
  , tenantUuid :: U.UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)
