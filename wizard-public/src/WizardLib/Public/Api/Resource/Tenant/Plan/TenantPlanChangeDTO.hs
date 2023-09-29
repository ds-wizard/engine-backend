module WizardLib.Public.Api.Resource.Tenant.Plan.TenantPlanChangeDTO where

import Data.Time
import GHC.Generics

data TenantPlanChangeDTO = TenantPlanChangeDTO
  { name :: String
  , users :: Maybe Int
  , since :: Maybe UTCTime
  , until :: Maybe UTCTime
  , test :: Bool
  }
  deriving (Generic)
