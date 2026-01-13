module Wizard.TestMigration where

import Data.Foldable (traverse_)

import Shared.ActionKey.Database.DAO.ActionKey.ActionKeyDAO
import Shared.Audit.Database.DAO.Audit.AuditDAO
import qualified Shared.Audit.Database.Migration.Development.Audit.AuditSchemaMigration as Audit
import Shared.Common.Constant.Tenant
import Shared.Component.Database.DAO.Component.ComponentDAO
import qualified Shared.Component.Database.Migration.Development.Component.ComponentSchemaMigration as Component
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Prefab.Database.DAO.Prefab.PrefabDAO
import qualified Shared.Prefab.Database.Migration.Development.Prefab.PrefabSchemaMigration as Prefab
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Wizard.Database.DAO.Feedback.FeedbackDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelMigrationDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelSecretDAO
import Wizard.Database.DAO.Plugin.PluginDAO
import Wizard.Database.DAO.Project.ProjectActionDAO
import Wizard.Database.DAO.Project.ProjectCommentDAO
import Wizard.Database.DAO.Project.ProjectCommentThreadDAO
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectEventDAO
import Wizard.Database.DAO.Project.ProjectFileDAO
import Wizard.Database.DAO.Project.ProjectImporterDAO
import Wizard.Database.DAO.Project.ProjectMigrationDAO
import Wizard.Database.DAO.Project.ProjectPermDAO
import Wizard.Database.DAO.Project.ProjectVersionDAO
import Wizard.Database.DAO.Registry.RegistryKnowledgeModelPackageDAO
import Wizard.Database.DAO.Registry.RegistryOrganizationDAO
import Wizard.Database.DAO.Registry.RegistryTemplateDAO
import Wizard.Database.DAO.Submission.SubmissionDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigAuthenticationDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigDashboardAndLoginScreenDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigKnowledgeModelDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigOrganizationDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigOwlDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigPrivacyAndSupportDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigProjectDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigRegistryDAO
import Wizard.Database.DAO.Tenant.Config.TenantConfigSubmissionDAO
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Database.DAO.Tenant.TenantLimitBundleDAO
import Wizard.Database.DAO.User.UserDAO
import qualified Wizard.Database.Migration.Development.ActionKey.ActionKeySchemaMigration as ActionKey
import qualified Wizard.Database.Migration.Development.Common.CommonSchemaMigration as Common
import qualified Wizard.Database.Migration.Development.Document.DocumentSchemaMigration as Document
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplateMigration
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateSchemaMigration as DocumentTemplate
import qualified Wizard.Database.Migration.Development.Feedback.FeedbackSchemaMigration as Feedback
import qualified Wizard.Database.Migration.Development.Instance.InstanceSchemaMigration as Instance
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelCacheSchemaMigration as KnowledgeModelCache
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorSchemaMigration as KnowledgeModelEditor
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelMigrationSchemaMigration as KnowledgeModelMigration
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageSchemaMigration as KnowledgeModelPackage
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelSecretSchemaMigration as KnowledgeModelSecret
import qualified Wizard.Database.Migration.Development.Locale.LocaleMigration as LocaleMigration
import qualified Wizard.Database.Migration.Development.Locale.LocaleSchemaMigration as Locale
import qualified Wizard.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration as PersistentCommand
import Wizard.Database.Migration.Development.Plugin.Data.Plugins
import qualified Wizard.Database.Migration.Development.Plugin.PluginSchemaMigration as Plugin
import qualified Wizard.Database.Migration.Development.Project.ProjectActionSchemaMigration as ProjectAction
import qualified Wizard.Database.Migration.Development.Project.ProjectImporterSchemaMigration as ProjectImporter
import qualified Wizard.Database.Migration.Development.Project.ProjectMigrationSchemaMigration as ProjectMigration
import qualified Wizard.Database.Migration.Development.Project.ProjectSchemaMigration as Project
import qualified Wizard.Database.Migration.Development.Registry.RegistrySchemaMigration as Registry
import qualified Wizard.Database.Migration.Development.Submission.SubmissionSchemaMigration as Submission
import Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs
import Wizard.Database.Migration.Development.Tenant.Data.TenantLimitBundles
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import qualified Wizard.Database.Migration.Development.Tenant.TenantSchemaMigration as Tenant
import Wizard.Database.Migration.Development.User.Data.UserTokens
import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Database.Migration.Development.User.UserSchemaMigration as User
import Wizard.Model.Cache.ServerCache
import Wizard.Model.Tenant.Config.TenantConfig
import WizardLib.Public.Database.DAO.ExternalLink.ExternalLinkUsageDAO
import WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigFeaturesDAO
import WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigLookAndFeelDAO
import WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigMailDAO
import WizardLib.Public.Database.DAO.User.UserGroupDAO
import WizardLib.Public.Database.DAO.User.UserGroupMembershipDAO
import WizardLib.Public.Database.DAO.User.UserTokenDAO
import WizardLib.Public.Database.DAO.User.UserTourDAO
import qualified WizardLib.Public.Database.Migration.Development.ExternalLink.ExternalLinkSchemaMigration as ExternalLink
import WizardLib.Public.Database.Migration.Development.Tenant.Data.TenantConfigs
import WizardLib.Public.Model.Tenant.Config.TenantConfig

import Wizard.Specs.Common

buildSchema appContext = do
  putStrLn "DB: dropping DB triggers"
  runInContext Document.dropTriggers appContext
  runInContext Project.dropTriggers appContext
  runInContext Locale.dropTriggers appContext
  putStrLn "DB: dropping DB functions"
  runInContext Project.dropFunctions appContext
  runInContext DocumentTemplate.dropFunctions appContext
  runInContext KnowledgeModelEditor.dropFunctions appContext
  runInContext KnowledgeModelPackage.dropFunctions appContext
  runInContext Common.dropFunctions appContext
  putStrLn "DB: dropping schema"
  runInContext ExternalLink.dropTables appContext
  runInContext Component.dropTables appContext
  runInContext Registry.dropTables appContext
  runInContext ProjectAction.dropTables appContext
  runInContext ProjectImporter.dropTables appContext
  runInContext Audit.dropTables appContext
  runInContext Prefab.dropTables appContext
  runInContext PersistentCommand.dropTables appContext
  runInContext Submission.dropTables appContext
  runInContext ActionKey.dropTables appContext
  runInContext Feedback.dropTables appContext
  runInContext KnowledgeModelMigration.dropTables appContext
  runInContext KnowledgeModelCache.dropTables appContext
  runInContext KnowledgeModelEditor.dropTables appContext
  runInContext Document.dropTables appContext
  runInContext ProjectMigration.dropTables appContext
  runInContext Project.dropTables appContext
  runInContext KnowledgeModelSecret.dropTables appContext
  runInContext KnowledgeModelPackage.dropTables appContext
  runInContext User.dropTables appContext
  runInContext Tenant.dropConfigTables appContext
  runInContext DocumentTemplate.dropTables appContext
  runInContext Locale.dropTables appContext
  runInContext Plugin.dropTables appContext
  runInContext Tenant.dropTables appContext
  runInContext Instance.dropTables appContext
  putStrLn "DB: Drop DB types"
  runInContext Common.dropTypes appContext
  -- 2. Create
  putStrLn "DB: Create DB types"
  runInContext Common.createTypes appContext
  putStrLn "DB: Creating schema"
  runInContext Instance.createTables appContext
  runInContext Tenant.createTables appContext
  runInContext Plugin.createTables appContext
  runInContext Locale.createTables appContext
  runInContext DocumentTemplate.createTables appContext
  runInContext Tenant.createConfigTables appContext
  runInContext User.createTables appContext
  runInContext KnowledgeModelPackage.createTables appContext
  runInContext KnowledgeModelSecret.createTables appContext
  runInContext ActionKey.createTables appContext
  runInContext Feedback.createTables appContext
  runInContext KnowledgeModelEditor.createTables appContext
  runInContext KnowledgeModelCache.createTables appContext
  runInContext Project.createTables appContext
  runInContext DocumentTemplate.createDraftDataTable appContext
  runInContext Document.createTables appContext
  runInContext ProjectMigration.createTables appContext
  runInContext KnowledgeModelMigration.createTables appContext
  runInContext Submission.createTables appContext
  runInContext PersistentCommand.createTables appContext
  runInContext Prefab.createTables appContext
  runInContext Audit.createTables appContext
  runInContext ProjectAction.createTables appContext
  runInContext ProjectImporter.createTables appContext
  runInContext Registry.createTables appContext
  runInContext Component.createTables appContext
  runInContext ExternalLink.createTables appContext
  putStrLn "DB: Creating DB functions"
  runInContext Common.createFunctions appContext
  runInContext KnowledgeModelPackage.createFunctions appContext
  runInContext KnowledgeModelEditor.createFunctions appContext
  runInContext DocumentTemplate.createFunctions appContext
  runInContext Project.createFunctions appContext
  putStrLn "DB: Creating missing foreign key constraints"
  runInContext User.createUserLocaleForeignKeyConstraint appContext
  putStrLn "DB: Creating triggers"
  runInContext Locale.createTriggers appContext
  runInContext Project.createTriggers appContext
  runInContext Document.createTriggers appContext
  putStrLn "DB-S3: Purging and creating schema"
  runInContext DocumentTemplateMigration.runS3Migration appContext
  runInContext LocaleMigration.runS3Migration appContext

resetDB appContext = do
  runInContext deleteExternalLinkUsages appContext
  runInContext deleteRegistryOrganizations appContext
  runInContext deleteRegistryPackages appContext
  runInContext deleteRegistryTemplates appContext
  runInContext deleteAudits appContext
  runInContext deletePrefabs appContext
  runInContext deletePersistentCommands appContext
  runInContext deleteSubmissions appContext
  runInContext deleteTenantConfigOwls appContext
  runInContext deleteTenantConfigMails appContext
  runInContext deleteTenantConfigFeatures appContext
  runInContext deleteTenantConfigSubmissions appContext
  runInContext deleteTenantConfigProjects appContext
  runInContext deleteTenantConfigKnowledgeModels appContext
  runInContext deleteTenantConfigRegistries appContext
  runInContext deleteTenantConfigLookAndFeels appContext
  runInContext deleteTenantConfigDashboardAndLoginScreens appContext
  runInContext deleteTenantConfigPrivacyAndSupports appContext
  runInContext deleteTenantConfigAuthentications appContext
  runInContext deleteTenantConfigOrganizations appContext
  runInContext deleteKnowledgeModelMigrations appContext
  runInContext deleteProjectMigrations appContext
  runInContext deleteFeedbacks appContext
  runInContext deleteActionKeys appContext
  runInContext deleteKnowledgeModelEditors appContext
  runInContext deleteDocuments appContext
  runInContext deleteDrafts appContext
  runInContext deleteProjectVersions appContext
  runInContext deleteProjectEvents appContext
  runInContext deleteProjectFiles appContext
  runInContext deleteProjectVersions appContext
  runInContext deleteProjectComments appContext
  runInContext deleteProjectCommentThreads appContext
  runInContext deleteProjectPerms appContext
  runInContext deleteProjects appContext
  runInContext deleteProjectActions appContext
  runInContext deleteProjectImporters appContext
  runInContext deleteDocumentTemplates appContext
  runInContext deleteKnowledgeModelSecrets appContext
  runInContext deletePackages appContext
  runInContext deleteUserTokens appContext
  runInContext deleteUserGroupMemberships appContext
  runInContext deleteTours appContext
  runInContext deleteUsers appContext
  runInContext deleteUserGroups appContext
  runInContext deleteLocales appContext
  runInContext deletePersistentCommands appContext
  runInContext deleteLimitBundles appContext
  runInContext deletePlugins appContext
  runInContext deleteTenants appContext
  runInContext (insertTenant defaultTenant) appContext
  runInContext (insertPlugin plugin1) appContext
  runInContext (insertPlugin differentPlugin1) appContext
  runInContext (insertLimitBundle defaultTenantLimitBundle) appContext
  runInContext (insertTenant differentTenant) appContext
  runInContext (insertLimitBundle differentTenantLimitBundle) appContext
  runInContext (insertTenantConfigOrganization defaultOrganization) appContext
  runInContext (insertTenantConfigAuthentication defaultAuthenticationEncrypted) appContext
  runInContext (insertTenantConfigAuthenticationExternalService defaultAuthExternalServiceEncrypted) appContext
  runInContext (insertTenantConfigPrivacyAndSupport defaultPrivacyAndSupport) appContext
  runInContext (insertTenantConfigDashboardAndLoginScreen defaultDashboardAndLoginScreen) appContext
  runInContext (insertTenantConfigDashboardAndLoginScreenAnnouncement defaultDashboardAndLoginScreenAnnouncement) appContext
  runInContext (insertTenantConfigLookAndFeel defaultLookAndFeel) appContext
  runInContext (insertTenantConfigLookAndFeelCustomMenuLink defaultLookAndFeelCustomLink) appContext
  runInContext (insertTenantConfigRegistry defaultRegistryEncrypted) appContext
  runInContext (insertTenantConfigKnowledgeModel defaultKnowledgeModelEncrypted) appContext
  runInContext (insertTenantConfigKnowledgeModelPublicPackagePattern defaultKnowledgeModelPublicPackagePattern) appContext
  runInContext (insertTenantConfigProject defaultProjectEncrypted) appContext
  runInContext (insertTenantConfigSubmission (defaultSubmission {services = []})) appContext
  runInContext (insertTenantConfigFeatures defaultFeatures) appContext
  runInContext (insertTenantConfigMail defaultMail) appContext
  runInContext (insertTenantConfigOwl defaultOwl) appContext
  runInContext (insertTenantConfigLookAndFeel (defaultLookAndFeel {tenantUuid = differentTenantUuid})) appContext
  runInContext (insertUser userSystem) appContext
  runInContext (insertUser userAlbert) appContext
  runInContext (insertUserToken albertToken) appContext
  runInContext (insertUser userCharles) appContext
  runInContext (insertPackage globalKmPackageEmpty) appContext
  runInContext (insertPackage globalKmPackage) appContext
  runInContext (insertPackage netherlandsKmPackage) appContext
  runInContext (insertPackage netherlandsKmPackageV2) appContext
  runInContext (insertPackage globalKmPackageEmpty) appContext
  runInContext (traverse_ insertPackageEvent globalKmPackageEmptyEvents) appContext
  runInContext (insertPackage globalKmPackage) appContext
  runInContext (traverse_ insertPackageEvent globalKmPackageEvents) appContext
  runInContext (insertPackage netherlandsKmPackage) appContext
  runInContext (traverse_ insertPackageEvent netherlandsKmPackageEvents) appContext
  runInContext (insertPackage netherlandsKmPackageV2) appContext
  runInContext (traverse_ insertPackageEvent netherlandsKmPackageV2Events) appContext
  runInContext (insertPackage differentPackage) appContext
  runInContext (traverse_ insertPackageEvent differentPackageEvents) appContext
  runInContext deleteComponents appContext
  return ()
