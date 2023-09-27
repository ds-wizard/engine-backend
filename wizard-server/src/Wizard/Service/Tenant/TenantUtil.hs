module Wizard.Service.Tenant.TenantUtil where

import Wizard.Api.Resource.Tenant.TenantDTO
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Tenant
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.TenantMapper

enhanceTenant :: Tenant -> AppContextM TenantDTO
enhanceTenant tenant = do
  tenantConfig <- getTenantConfigByUuid tenant.uuid
  return $ toDTO tenant tenantConfig.lookAndFeel.logoUrl tenantConfig.lookAndFeel.primaryColor
