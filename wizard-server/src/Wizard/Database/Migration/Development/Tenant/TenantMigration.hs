module Wizard.Database.Migration.Development.Tenant.TenantMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Tenant.Config.TenantConfigAuthenticationDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigDashboardAndLoginScreenDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigKnowledgeModelDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigOwlDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigPrivacyAndSupportDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigQuestionnaireDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigRegistryDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigSubmissionDAO
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Database.DAO.Tenant.TenantLimitBundleDAO
import Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs
import Wizard.Database.Migration.Development.Tenant.Data.TenantLimitBundles
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Tenant.Config.TenantConfig
import WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigFeaturesDAO
import WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigLookAndFeelDAO
import WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigMailDAO
import WizardLib.Public.Database.Migration.Development.Tenant.Data.TenantConfigs

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Tenant/Tenant) started"
  runTenantMigration
  runConfigMigration
  runLimitMigration
  logInfo _CMP_MIGRATION "(Tenant/Tenant) ended"

runTenantMigration :: AppContextM ()
runTenantMigration = do
  deleteTenants
  insertTenant defaultTenant
  insertTenant differentTenant
  return ()

runConfigMigration :: AppContextM ()
runConfigMigration = do
  deleteTenantConfigOrganizations
  insertTenantConfigOrganization defaultOrganization
  insertTenantConfigAuthentication defaultAuthenticationEncrypted
  insertTenantConfigAuthenticationExternalService defaultAuthExternalServiceEncrypted
  insertTenantConfigPrivacyAndSupport defaultPrivacyAndSupport
  insertTenantConfigDashboardAndLoginScreen defaultDashboardAndLoginScreen
  insertTenantConfigDashboardAndLoginScreenAnnouncement defaultDashboardAndLoginScreenAnnouncement
  insertTenantConfigLookAndFeel defaultLookAndFeel
  insertTenantConfigLookAndFeelCustomMenuLink defaultLookAndFeelCustomLink
  insertTenantConfigRegistry defaultRegistryEncrypted
  insertTenantConfigKnowledgeModel defaultKnowledgeModelEncrypted
  insertTenantConfigKnowledgeModelPublicPackagePattern defaultKnowledgeModelPublicPackagePattern
  insertTenantConfigQuestionnaire defaultQuestionnaireEncrypted
  insertTenantConfigSubmission (defaultSubmission {services = []})
  insertTenantConfigFeatures defaultFeatures
  insertTenantConfigMail defaultMail
  insertTenantConfigOwl defaultOwl
  return ()

runLimitMigration :: AppContextM ()
runLimitMigration = do
  deleteLimitBundles
  insertLimitBundle defaultTenantLimitBundle
  insertLimitBundle differentTenantLimitBundle
  return ()
