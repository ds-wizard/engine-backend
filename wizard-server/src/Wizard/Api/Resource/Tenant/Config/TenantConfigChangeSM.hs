module Wizard.Api.Resource.Tenant.Config.TenantConfigChangeSM where

import Data.Swagger

import Shared.Common.Util.Swagger
import Wizard.Api.Resource.Tenant.Config.TenantConfigChangeDTO
import Wizard.Api.Resource.Tenant.Config.TenantConfigChangeJM ()
import Wizard.Api.Resource.Tenant.Config.TenantConfigSM ()
import Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs
import Wizard.Service.Tenant.Config.ConfigMapper

instance ToSchema TenantConfigChangeDTO where
  declareNamedSchema = toSwagger (toChangeDTO defaultTenantConfig)
