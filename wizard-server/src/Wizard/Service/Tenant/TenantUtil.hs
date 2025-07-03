module Wizard.Service.Tenant.TenantUtil where

import Wizard.Api.Resource.Tenant.TenantDTO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Tenant.Tenant
import Wizard.Service.Tenant.TenantMapper
import WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigLookAndFeelDAO
import WizardLib.Public.Model.Tenant.Config.TenantConfig

enhanceTenant :: Tenant -> AppContextM TenantDTO
enhanceTenant tenant = do
  tcLookAndFeel <- findTenantConfigLookAndFeelByUuid tenant.uuid
  return $ toDTO tenant tcLookAndFeel.logoUrl tcLookAndFeel.primaryColor
