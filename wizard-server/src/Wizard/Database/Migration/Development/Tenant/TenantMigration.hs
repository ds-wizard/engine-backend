module Wizard.Database.Migration.Development.Tenant.TenantMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Tenant.TenantConfigDAO
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Database.DAO.Tenant.TenantLimitBundleDAO
import Wizard.Database.DAO.Tenant.TenantPlanDAO
import Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs
import Wizard.Database.Migration.Development.Tenant.Data.TenantLimitBundles
import Wizard.Database.Migration.Development.Tenant.Data.TenantPlans
import Wizard.Database.Migration.Development.Tenant.Data.Tenants

runMigration = do
  logInfo _CMP_MIGRATION "(Tenant/Tenant) started"
  runTenantMigration
  runConfigMigration
  logInfo _CMP_MIGRATION "(Tenant/Tenant) ended"

runTenantMigration = do
  deleteTenants
  insertTenant defaultTenant
  insertTenant differentTenant

runConfigMigration = do
  deleteTenantConfigs
  insertTenantConfig defaultTenantConfigEncrypted
  insertTenantConfig differentTenantConfigEncrypted

runPlanMigration = do
  deletePlans
  insertPlan standardPlanExpired
  insertPlan standardPlan

runLimitMigration = do
  deleteLimitBundles
  insertLimitBundle defaultTenantLimitBundle
  insertLimitBundle differentTenantLimitBundle
