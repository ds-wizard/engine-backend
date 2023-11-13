module Wizard.Api.Resource.Tenant.TenantChangeDTO where

import GHC.Generics

data TenantChangeDTO = TenantChangeDTO
  { tenantId :: String
  , name :: String
  }
  deriving (Generic)
