module Wizard.Api.Resource.Tenant.Usage.TenantUsageDTO where

import GHC.Generics
import GHC.Int

data TenantUsageDTO = TenantUsageDTO
  { users :: TenantUsageEntryDTO
  , activeUsers :: TenantUsageEntryDTO
  , knowledgeModels :: TenantUsageEntryDTO
  , branches :: TenantUsageEntryDTO
  , documentTemplates :: TenantUsageEntryDTO
  , documentTemplateDrafts :: TenantUsageEntryDTO
  , questionnaires :: TenantUsageEntryDTO
  , documents :: TenantUsageEntryDTO
  , locales :: TenantUsageEntryDTO
  , storage :: TenantUsageEntryDTO
  }
  deriving (Show, Eq, Generic)

data TenantUsageEntryDTO = TenantUsageEntryDTO
  { current :: Int64
  , max :: Maybe Int64
  }
  deriving (Show, Eq, Generic)
