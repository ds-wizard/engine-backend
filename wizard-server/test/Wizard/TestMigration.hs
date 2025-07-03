module Wizard.TestMigration where

import Shared.ActionKey.Database.DAO.ActionKey.ActionKeyDAO
import Shared.Audit.Database.DAO.Audit.AuditDAO
import qualified Shared.Audit.Database.Migration.Development.Audit.AuditSchemaMigration as Audit
import Shared.Common.Constant.Tenant
import Shared.Component.Database.DAO.Component.ComponentDAO
import qualified Shared.Component.Database.Migration.Development.Component.ComponentSchemaMigration as Component
import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Prefab.Database.DAO.Prefab.PrefabDAO
import qualified Shared.Prefab.Database.Migration.Development.Prefab.PrefabSchemaMigration as Prefab
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Branch.BranchDataDAO
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Wizard.Database.DAO.Feedback.FeedbackDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelCacheDAO
import qualified Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO as KM_MigratorDAO
import qualified Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO as QTN_MigratorDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentThreadDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireEventDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireFileDAO
import Wizard.Database.DAO.Questionnaire.QuestionnairePermDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireVersionDAO
import Wizard.Database.DAO.QuestionnaireAction.QuestionnaireActionDAO
import Wizard.Database.DAO.QuestionnaireImporter.QuestionnaireImporterDAO
import Wizard.Database.DAO.Registry.RegistryOrganizationDAO
import Wizard.Database.DAO.Registry.RegistryPackageDAO
import Wizard.Database.DAO.Registry.RegistryTemplateDAO
import Wizard.Database.DAO.Submission.SubmissionDAO
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
import Wizard.Database.DAO.User.UserDAO
import qualified Wizard.Database.Migration.Development.ActionKey.ActionKeySchemaMigration as ActionKey
import qualified Wizard.Database.Migration.Development.Branch.BranchSchemaMigration as Branch
import qualified Wizard.Database.Migration.Development.Common.CommonSchemaMigration as Common
import qualified Wizard.Database.Migration.Development.Document.DocumentSchemaMigration as Document
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplateMigration
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateSchemaMigration as DocumentTemplate
import qualified Wizard.Database.Migration.Development.Feedback.FeedbackSchemaMigration as Feedback
import qualified Wizard.Database.Migration.Development.Instance.InstanceSchemaMigration as Instance
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelSchemaMigration as KnowledgeModel
import qualified Wizard.Database.Migration.Development.Locale.LocaleMigration as LocaleMigration
import qualified Wizard.Database.Migration.Development.Locale.LocaleSchemaMigration as Locale
import qualified Wizard.Database.Migration.Development.Migration.KnowledgeModel.MigratorSchemaMigration as KnowledgeModelMigrator
import qualified Wizard.Database.Migration.Development.Migration.Questionnaire.MigratorSchemaMigration as QuestionnaireMigrator
import Wizard.Database.Migration.Development.Package.Data.Packages
import qualified Wizard.Database.Migration.Development.Package.PackageSchemaMigration as Package
import qualified Wizard.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration as PersistentCommand
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireSchemaMigration as Questionnaire
import qualified Wizard.Database.Migration.Development.QuestionnaireAction.QuestionnaireActionSchemaMigration as QuestionnaireAction
import qualified Wizard.Database.Migration.Development.QuestionnaireImporter.QuestionnaireImporterSchemaMigration as QuestionnaireImporter
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
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.Public.Database.DAO.ExternalLink.ExternalLinkUsageDAO
import WizardLib.Public.Database.DAO.Tenant.Config.TenantConfigAiAssistantDAO
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
  runInContext Locale.dropTriggers appContext
  runInContext Questionnaire.dropTriggers appContext
  putStrLn "DB: dropping DB functions"
  runInContext Branch.dropFunctions appContext
  runInContext Package.dropFunctions appContext
  runInContext Common.dropFunctions appContext
  putStrLn "DB: dropping schema"
  runInContext Tenant.dropConfigTables appContext
  runInContext ExternalLink.dropTables appContext
  runInContext KnowledgeModel.dropTables appContext
  runInContext Component.dropTables appContext
  runInContext Registry.dropTables appContext
  runInContext QuestionnaireAction.dropTables appContext
  runInContext QuestionnaireImporter.dropTables appContext
  runInContext Audit.dropTables appContext
  runInContext Prefab.dropTables appContext
  runInContext PersistentCommand.dropTables appContext
  runInContext Submission.dropTables appContext
  runInContext ActionKey.dropTables appContext
  runInContext Feedback.dropTables appContext
  runInContext KnowledgeModelMigrator.dropTables appContext
  runInContext Branch.dropTables appContext
  runInContext Document.dropTables appContext
  runInContext QuestionnaireMigrator.dropTables appContext
  runInContext Questionnaire.dropTables appContext
  runInContext DocumentTemplate.dropTables appContext
  runInContext Package.dropTables appContext
  runInContext User.dropTables appContext
  runInContext Locale.dropTables appContext
  runInContext Tenant.dropTables appContext
  runInContext Instance.dropTables appContext
  putStrLn "DB: Creating schema"
  runInContext Instance.createTables appContext
  runInContext Tenant.createTables appContext
  runInContext Locale.createTables appContext
  runInContext User.createTables appContext
  runInContext DocumentTemplate.createTables appContext
  runInContext Package.createTables appContext
  runInContext ActionKey.createTables appContext
  runInContext Feedback.createTables appContext
  runInContext Branch.createTables appContext
  runInContext Questionnaire.createTables appContext
  runInContext DocumentTemplate.createDraftDataTable appContext
  runInContext Document.createTables appContext
  runInContext QuestionnaireMigrator.createTables appContext
  runInContext KnowledgeModelMigrator.createTables appContext
  runInContext Submission.createTables appContext
  runInContext PersistentCommand.createTables appContext
  runInContext Prefab.createTables appContext
  runInContext Audit.createTables appContext
  runInContext QuestionnaireAction.createTables appContext
  runInContext QuestionnaireImporter.createTables appContext
  runInContext Registry.createTables appContext
  runInContext Component.createTables appContext
  runInContext KnowledgeModel.createTables appContext
  runInContext ExternalLink.createTables appContext
  runInContext Tenant.createConfigTables appContext
  putStrLn "DB: Creating DB functions"
  runInContext Common.createFunctions appContext
  runInContext Package.createFunctions appContext
  runInContext Branch.createFunctions appContext
  putStrLn "DB: Creating missing foregign key constraints"
  runInContext User.createUserLocaleForeignKeyConstraint appContext
  putStrLn "DB: Creating triggers"
  runInContext Locale.createTriggers appContext
  runInContext Questionnaire.createTriggers appContext
  putStrLn "DB-S3: Purging and creating schema"
  runInContext DocumentTemplateMigration.runS3Migration appContext
  runInContext LocaleMigration.runS3Migration appContext

resetDB appContext = do
  runInContext deleteExternalLinkUsages appContext
  runInContext deleteKnowledgeModelCaches appContext
  runInContext deleteRegistryOrganizations appContext
  runInContext deleteRegistryPackages appContext
  runInContext deleteRegistryTemplates appContext
  runInContext deleteAudits appContext
  runInContext deletePrefabs appContext
  runInContext deletePersistentCommands appContext
  runInContext deleteSubmissions appContext
  runInContext deleteTenantConfigOwls appContext
  runInContext deleteTenantConfigMails appContext
  runInContext deleteTenantConfigAiAssistants appContext
  runInContext deleteTenantConfigSubmissions appContext
  runInContext deleteTenantConfigQuestionnaires appContext
  runInContext deleteTenantConfigKnowledgeModels appContext
  runInContext deleteTenantConfigRegistries appContext
  runInContext deleteTenantConfigLookAndFeels appContext
  runInContext deleteTenantConfigDashboardAndLoginScreens appContext
  runInContext deleteTenantConfigPrivacyAndSupports appContext
  runInContext deleteTenantConfigAuthentications appContext
  runInContext deleteTenantConfigOrganizations appContext
  runInContext KM_MigratorDAO.deleteMigratorStates appContext
  runInContext QTN_MigratorDAO.deleteMigratorStates appContext
  runInContext deleteFeedbacks appContext
  runInContext deleteActionKeys appContext
  runInContext deleteBranchDatas appContext
  runInContext deleteBranches appContext
  runInContext deleteDocuments appContext
  runInContext deleteDrafts appContext
  runInContext deleteQuestionnaireVersions appContext
  runInContext deleteQuestionnaireEvents appContext
  runInContext deleteQuestionnaireFiles appContext
  runInContext deleteQuestionnaireVersions appContext
  runInContext deleteQuestionnaireComments appContext
  runInContext deleteQuestionnaireCommentThreads appContext
  runInContext deleteQuestionnairePerms appContext
  runInContext deleteQuestionnaires appContext
  runInContext deleteQuestionnaireActions appContext
  runInContext deleteQuestionnaireImporters appContext
  runInContext deleteDocumentTemplates appContext
  runInContext deletePackages appContext
  runInContext deleteUserTokens appContext
  runInContext deleteUserGroupMemberships appContext
  runInContext deleteTours appContext
  runInContext deleteUsers appContext
  runInContext deleteUserGroups appContext
  runInContext deleteLocales appContext
  runInContext deletePersistentCommands appContext
  runInContext deleteLimitBundles appContext
  runInContext deleteTenants appContext
  runInContext (insertTenant defaultTenant) appContext
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
  runInContext (insertTenantConfigQuestionnaire defaultQuestionnaireEncrypted) appContext
  runInContext (insertTenantConfigSubmission (defaultSubmission {services = []})) appContext
  runInContext (insertTenantConfigAiAssistant defaultAiAssistant) appContext
  runInContext (insertTenantConfigMail defaultMail) appContext
  runInContext (insertTenantConfigOwl defaultOwl) appContext
  runInContext (insertTenantConfigLookAndFeel (defaultLookAndFeel {tenantUuid = differentTenantUuid})) appContext
  runInContext (insertUser userSystem) appContext
  runInContext (insertUser userAlbert) appContext
  runInContext (insertUserToken albertToken) appContext
  runInContext (insertUser userCharles) appContext
  runInContext (insertPackage globalPackageEmpty) appContext
  runInContext (insertPackage globalPackage) appContext
  runInContext (insertPackage netherlandsPackage) appContext
  runInContext (insertPackage netherlandsPackageV2) appContext
  runInContext (insertPackage differentPackage) appContext
  runInContext deleteComponents appContext
  return ()
