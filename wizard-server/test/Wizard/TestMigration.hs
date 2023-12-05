module Wizard.TestMigration where

import Shared.ActionKey.Database.DAO.ActionKey.ActionKeyDAO
import Shared.Audit.Database.DAO.Audit.AuditDAO
import qualified Shared.Audit.Database.Migration.Development.Audit.AuditSchemaMigration as ADT_Schema
import Shared.Component.Database.DAO.Component.ComponentDAO
import qualified Shared.Component.Database.Migration.Development.Component.ComponentSchemaMigration as CMP_Schema
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.Prefab.Database.DAO.Prefab.PrefabDAO
import qualified Shared.Prefab.Database.Migration.Development.Prefab.PrefabSchemaMigration as PF_Schema
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Branch.BranchDataDAO
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Wizard.Database.DAO.Feedback.FeedbackDAO
import Wizard.Database.DAO.Locale.LocaleDAO
import qualified Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO as KM_MigratorDAO
import qualified Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO as QTN_MigratorDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentThreadDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Questionnaire.QuestionnairePermDAO
import Wizard.Database.DAO.QuestionnaireAction.QuestionnaireActionDAO
import Wizard.Database.DAO.QuestionnaireImporter.QuestionnaireImporterDAO
import Wizard.Database.DAO.Registry.RegistryOrganizationDAO
import Wizard.Database.DAO.Registry.RegistryPackageDAO
import Wizard.Database.DAO.Registry.RegistryTemplateDAO
import Wizard.Database.DAO.Submission.SubmissionDAO
import Wizard.Database.DAO.Tenant.TenantConfigDAO
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Database.DAO.Tenant.TenantLimitBundleDAO
import Wizard.Database.DAO.User.UserDAO
import qualified Wizard.Database.Migration.Development.ActionKey.ActionKeySchemaMigration as ACK_Schema
import qualified Wizard.Database.Migration.Development.BookReference.BookReferenceSchemaMigration as BR_Schema
import qualified Wizard.Database.Migration.Development.Branch.BranchSchemaMigration as B_Schema
import qualified Wizard.Database.Migration.Development.Common.CommonSchemaMigration as CMN_Schema
import qualified Wizard.Database.Migration.Development.Document.DocumentSchemaMigration as DOC_Schema
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateSchemaMigration as TML_Schema
import qualified Wizard.Database.Migration.Development.Feedback.FeedbackSchemaMigration as F_Schema
import qualified Wizard.Database.Migration.Development.Instance.InstanceSchemaMigration as INS_Schema
import qualified Wizard.Database.Migration.Development.Locale.LocaleMigration as LOC
import qualified Wizard.Database.Migration.Development.Locale.LocaleSchemaMigration as LOC_Schema
import qualified Wizard.Database.Migration.Development.Migration.KnowledgeModel.MigratorSchemaMigration as KM_MIG_Schema
import qualified Wizard.Database.Migration.Development.Migration.Questionnaire.MigratorSchemaMigration as QTN_MIG_Schema
import Wizard.Database.Migration.Development.Package.Data.Packages
import qualified Wizard.Database.Migration.Development.Package.PackageSchemaMigration as PKG_Schema
import qualified Wizard.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration as PC_Schema
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireSchemaMigration as QTN_Schema
import qualified Wizard.Database.Migration.Development.QuestionnaireAction.QuestionnaireActionSchemaMigration as QA_Schema
import qualified Wizard.Database.Migration.Development.QuestionnaireImporter.QuestionnaireImporterSchemaMigration as QI_Schema
import qualified Wizard.Database.Migration.Development.Registry.RegistrySchemaMigration as R_Schema
import qualified Wizard.Database.Migration.Development.Submission.SubmissionSchemaMigration as SUB_Schema
import Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs
import Wizard.Database.Migration.Development.Tenant.Data.TenantLimitBundles
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import qualified Wizard.Database.Migration.Development.Tenant.TenantSchemaMigration as TNT_Schema
import Wizard.Database.Migration.Development.User.Data.UserTokens
import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Database.Migration.Development.User.UserSchemaMigration as U_Schema
import Wizard.Model.Cache.ServerCache
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.Public.Database.DAO.Tenant.TenantPlanDAO
import WizardLib.Public.Database.DAO.User.UserGroupDAO
import WizardLib.Public.Database.DAO.User.UserGroupMembershipDAO
import WizardLib.Public.Database.DAO.User.UserTokenDAO

import Wizard.Specs.Common

buildSchema appContext = do
  putStrLn "DB: dropping DB functions"
  runInContext LOC_Schema.dropFunctions appContext
  runInContext B_Schema.dropFunctions appContext
  runInContext TML_Schema.dropFunctions appContext
  runInContext PKG_Schema.dropFunctions appContext
  runInContext CMN_Schema.dropFunctions appContext
  putStrLn "DB: dropping schema"
  runInContext CMP_Schema.dropTables appContext
  runInContext LOC_Schema.dropTables appContext
  runInContext R_Schema.dropTables appContext
  runInContext QA_Schema.dropTables appContext
  runInContext QI_Schema.dropTables appContext
  runInContext ADT_Schema.dropTables appContext
  runInContext PF_Schema.dropTables appContext
  runInContext PC_Schema.dropTables appContext
  runInContext SUB_Schema.dropTables appContext
  runInContext ACK_Schema.dropTables appContext
  runInContext BR_Schema.dropTables appContext
  runInContext F_Schema.dropTables appContext
  runInContext KM_MIG_Schema.dropTables appContext
  runInContext B_Schema.dropTables appContext
  runInContext DOC_Schema.dropTables appContext
  runInContext QTN_MIG_Schema.dropTables appContext
  runInContext QTN_Schema.dropTables appContext
  runInContext TML_Schema.dropTables appContext
  runInContext PKG_Schema.dropTables appContext
  runInContext U_Schema.dropTables appContext
  runInContext TNT_Schema.dropTables appContext
  runInContext INS_Schema.dropTables appContext
  putStrLn "DB: Creating schema"
  runInContext INS_Schema.createTables appContext
  runInContext TNT_Schema.createTables appContext
  runInContext U_Schema.createTables appContext
  runInContext TML_Schema.createTables appContext
  runInContext PKG_Schema.createTables appContext
  runInContext ACK_Schema.createTables appContext
  runInContext BR_Schema.createTables appContext
  runInContext F_Schema.createTables appContext
  runInContext B_Schema.createTables appContext
  runInContext QTN_Schema.createTables appContext
  runInContext TML_Schema.createDraftDataTable appContext
  runInContext DOC_Schema.createTables appContext
  runInContext QTN_MIG_Schema.createTables appContext
  runInContext KM_MIG_Schema.createTables appContext
  runInContext SUB_Schema.createTables appContext
  runInContext PC_Schema.createTables appContext
  runInContext PF_Schema.createTables appContext
  runInContext ADT_Schema.createTables appContext
  runInContext QA_Schema.createTables appContext
  runInContext QI_Schema.createTables appContext
  runInContext R_Schema.createTables appContext
  runInContext LOC_Schema.createTables appContext
  runInContext CMP_Schema.createTables appContext
  putStrLn "DB: Creating DB functions"
  runInContext CMN_Schema.createFunctions appContext
  runInContext PKG_Schema.createFunctions appContext
  runInContext TML_Schema.createFunctions appContext
  runInContext B_Schema.createFunctions appContext
  runInContext LOC_Schema.createFunctions appContext
  putStrLn "DB-S3: Purging and creating schema"
  runInContext TML.runS3Migration appContext
  runInContext LOC.runS3Migration appContext

resetDB appContext = do
  runInContext deleteLocales appContext
  runInContext deleteRegistryOrganizations appContext
  runInContext deleteRegistryPackages appContext
  runInContext deleteRegistryTemplates appContext
  runInContext deleteAudits appContext
  runInContext deletePrefabs appContext
  runInContext deletePersistentCommands appContext
  runInContext deleteSubmissions appContext
  runInContext deleteTenantConfigs appContext
  runInContext (insertTenantConfig defaultTenantConfigEncrypted) appContext
  runInContext (insertTenantConfig differentTenantConfigEncrypted) appContext
  runInContext KM_MigratorDAO.deleteMigratorStates appContext
  runInContext QTN_MigratorDAO.deleteMigratorStates appContext
  runInContext deleteFeedbacks appContext
  runInContext deleteActionKeys appContext
  runInContext deleteBranchDatas appContext
  runInContext deleteBranches appContext
  runInContext deleteDocuments appContext
  runInContext deleteDrafts appContext
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
  runInContext deleteUsers appContext
  runInContext deleteUserGroups appContext
  runInContext deleteLimitBundles appContext
  runInContext deleteTenantPlans appContext
  runInContext deleteTenants appContext
  runInContext (insertTenant defaultTenant) appContext
  runInContext (insertLimitBundle defaultTenantLimitBundle) appContext
  runInContext (insertTenant differentTenant) appContext
  runInContext (insertLimitBundle differentTenantLimitBundle) appContext
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
