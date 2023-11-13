module Wizard.Api.Resource.Tenant.TenantCreateDTO where

import GHC.Generics

data TenantCreateDTO = TenantCreateDTO
  { tenantId :: String
  , tenantName :: String
  , firstName :: String
  , lastName :: String
  , email :: String
  , password :: String
  }
  deriving (Generic)
