module Wizard.Api.Resource.Tenant.TenantDetailDTO where

import Data.Time
import qualified Data.UUID as U
import GHC.Generics

import Wizard.Api.Resource.Tenant.Usage.TenantUsageDTO
import Wizard.Api.Resource.User.UserDTO
import WizardLib.Public.Model.Tenant.Plan.TenantPlan

data TenantDetailDTO = TenantDetailDTO
  { uuid :: U.UUID
  , tenantId :: String
  , name :: String
  , serverDomain :: String
  , serverUrl :: String
  , clientUrl :: String
  , enabled :: Bool
  , logoUrl :: Maybe String
  , primaryColor :: Maybe String
  , plans :: [TenantPlan]
  , usage :: TenantUsageDTO
  , users :: [UserDTO]
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving (Generic)
