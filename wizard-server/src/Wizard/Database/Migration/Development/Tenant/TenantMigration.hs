module Wizard.Database.Migration.Development.Tenant.TenantMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Tenant.Config.TenantConfigSubmissionDAO
import Wizard.Database.DAO.Tenant.TenantConfigDAO
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Database.DAO.Tenant.TenantLimitBundleDAO
import Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs
import Wizard.Database.Migration.Development.Tenant.Data.TenantLimitBundles
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Tenant/Tenant) started"
  runTenantMigration
  runConfigMigration
  logInfo _CMP_MIGRATION "(Tenant/Tenant) ended"

runTenantMigration :: AppContextM ()
runTenantMigration = do
  deleteTenants
  insertTenant defaultTenant
  insertTenant differentTenant
  return ()

runConfigMigration :: AppContextM ()
runConfigMigration = do
  deleteTenantConfigs
  insertTenantConfig defaultTenantConfigEncrypted
  insertTenantConfigSubmission defaultSubmission
  insertTenantConfig differentTenantConfigEncrypted
  return ()

runLimitMigration :: AppContextM ()
runLimitMigration = do
  deleteLimitBundles
  insertLimitBundle defaultTenantLimitBundle
  insertLimitBundle differentTenantLimitBundle
  return ()
