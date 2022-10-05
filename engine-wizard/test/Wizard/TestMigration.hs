module Wizard.TestMigration where

import Shared.Database.DAO.Package.PackageDAO
import Shared.Database.DAO.Template.TemplateDAO
import Shared.Database.Migration.Development.Package.Data.Packages
import Wizard.Database.DAO.ActionKey.ActionKeyDAO
import Wizard.Database.DAO.App.AppDAO
import Wizard.Database.DAO.Audit.AuditDAO
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Branch.BranchDataDAO
import Wizard.Database.DAO.Config.AppConfigDAO
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Feedback.FeedbackDAO
import Wizard.Database.DAO.Limit.AppLimitDAO
import qualified Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO as KM_MigratorDAO
import qualified Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO as QTN_MigratorDAO
import Wizard.Database.DAO.PersistentCommand.PersistentCommandDAO
import Wizard.Database.DAO.Plan.AppPlanDAO
import Wizard.Database.DAO.Prefab.PrefabDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentThreadDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.QuestionnaireImporter.QuestionnaireImporterDAO
import Wizard.Database.DAO.Registry.RegistryOrganizationDAO
import Wizard.Database.DAO.Registry.RegistryPackageDAO
import Wizard.Database.DAO.Registry.RegistryTemplateDAO
import Wizard.Database.DAO.Submission.SubmissionDAO
import Wizard.Database.DAO.User.UserDAO
import qualified Wizard.Database.Migration.Development.Acl.AclSchemaMigration as ACL_Schema
import qualified Wizard.Database.Migration.Development.ActionKey.ActionKeySchemaMigration as ACK_Schema
import qualified Wizard.Database.Migration.Development.App.AppSchemaMigration as A_Schema
import Wizard.Database.Migration.Development.App.Data.Apps
import qualified Wizard.Database.Migration.Development.Audit.AuditSchemaMigration as ADT_Schema
import qualified Wizard.Database.Migration.Development.BookReference.BookReferenceSchemaMigration as BR_Schema
import qualified Wizard.Database.Migration.Development.Branch.BranchSchemaMigration as B_Schema
import qualified Wizard.Database.Migration.Development.Config.AppConfigSchemaMigration as CFG_Schema
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import qualified Wizard.Database.Migration.Development.Document.DocumentSchemaMigration as DOC_Schema
import qualified Wizard.Database.Migration.Development.Feedback.FeedbackSchemaMigration as F_Schema
import qualified Wizard.Database.Migration.Development.Limit.AppLimitSchemaMigration as AL_Schema
import Wizard.Database.Migration.Development.Limit.Data.AppLimits
import qualified Wizard.Database.Migration.Development.Migration.KnowledgeModel.MigratorSchemaMigration as KM_MIG_Schema
import qualified Wizard.Database.Migration.Development.Migration.Questionnaire.MigratorSchemaMigration as QTN_MIG_Schema
import Wizard.Database.Migration.Development.Package.Data.Packages
import qualified Wizard.Database.Migration.Development.Package.PackageSchemaMigration as PKG_Schema
import qualified Wizard.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration as PC_Schema
import qualified Wizard.Database.Migration.Development.Plan.AppPlanSchemaMigration as AP_Schema
import qualified Wizard.Database.Migration.Development.Prefab.PrefabSchemaMigration as PF_Schema
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireSchemaMigration as QTN_Schema
import qualified Wizard.Database.Migration.Development.QuestionnaireImporter.QuestionnaireImporterSchemaMigration as QI_Schema
import qualified Wizard.Database.Migration.Development.Registry.RegistrySchemaMigration as R_Schema
import qualified Wizard.Database.Migration.Development.Submission.SubmissionSchemaMigration as SUB_Schema
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML
import qualified Wizard.Database.Migration.Development.Template.TemplateSchemaMigration as TML_Schema
import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Database.Migration.Development.User.UserSchemaMigration as U_Schema

import Wizard.Specs.Common

buildSchema appContext = do
  putStrLn "DB: dropping DB functions"
  runInContext B_Schema.dropFunctions appContext
  runInContext PKG_Schema.dropFunctions appContext
  putStrLn "DB: dropping schema"
  runInContext R_Schema.dropTables appContext
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
  runInContext ACL_Schema.dropTables appContext
  runInContext U_Schema.dropTables appContext
  runInContext CFG_Schema.dropTables appContext
  runInContext AP_Schema.dropTables appContext
  runInContext AL_Schema.dropTables appContext
  runInContext A_Schema.dropTables appContext
  putStrLn "DB: Creating schema"
  runInContext A_Schema.createTables appContext
  runInContext AL_Schema.createTables appContext
  runInContext AP_Schema.createTables appContext
  runInContext U_Schema.createTables appContext
  runInContext ACL_Schema.createTables appContext
  runInContext TML_Schema.createTables appContext
  runInContext PKG_Schema.createTables appContext
  runInContext CFG_Schema.createTables appContext
  runInContext ACK_Schema.createTables appContext
  runInContext BR_Schema.createTables appContext
  runInContext F_Schema.createTables appContext
  runInContext B_Schema.createTables appContext
  runInContext QTN_Schema.createTables appContext
  runInContext DOC_Schema.createTables appContext
  runInContext QTN_MIG_Schema.createTables appContext
  runInContext KM_MIG_Schema.createTables appContext
  runInContext SUB_Schema.createTables appContext
  runInContext PC_Schema.createTables appContext
  runInContext PF_Schema.createTables appContext
  runInContext ADT_Schema.createTables appContext
  runInContext QI_Schema.createTables appContext
  runInContext R_Schema.createTables appContext
  putStrLn "DB: Creating DB functions"
  runInContext PKG_Schema.createFunctions appContext
  runInContext B_Schema.createFunctions appContext
  putStrLn "DB-S3: Purging and creating schema"
  runInContext TML.runS3Migration appContext

resetDB appContext = do
  runInContext deleteRegistryOrganizations appContext
  runInContext deleteRegistryPackages appContext
  runInContext deleteRegistryTemplates appContext
  runInContext deleteAudits appContext
  runInContext deletePrefabs appContext
  runInContext deletePersistentCommands appContext
  runInContext deleteSubmissions appContext
  runInContext deleteAppConfigs appContext
  runInContext (insertAppConfig defaultAppConfigEncrypted) appContext
  runInContext (insertAppConfig differentAppConfigEncrypted) appContext
  runInContext KM_MigratorDAO.deleteMigratorStates appContext
  runInContext QTN_MigratorDAO.deleteMigratorStates appContext
  runInContext deleteFeedbacks appContext
  runInContext deleteActionKeys appContext
  runInContext deleteBranchDatas appContext
  runInContext deleteBranches appContext
  runInContext deleteDocuments appContext
  runInContext deleteQuestionnaireComments appContext
  runInContext deleteQuestionnaireCommentThreads appContext
  runInContext deleteQuestionnaires appContext
  runInContext deleteQuestionnaireImporters appContext
  runInContext deleteTemplates appContext
  runInContext deletePackages appContext
  runInContext deleteUsers appContext
  runInContext deleteAppLimits appContext
  runInContext deleteAppPlans appContext
  runInContext deleteApps appContext
  runInContext (insertApp defaultApp) appContext
  runInContext (insertAppLimit defaultAppLimit) appContext
  runInContext (insertApp differentApp) appContext
  runInContext (insertAppLimit differentAppLimit) appContext
  runInContext (insertUser userSystem) appContext
  runInContext (insertUser userAlbert) appContext
  runInContext (insertUser userCharles) appContext
  runInContext (insertPackage globalPackageEmpty) appContext
  runInContext (insertPackage globalPackage) appContext
  runInContext (insertPackage netherlandsPackage) appContext
  runInContext (insertPackage netherlandsPackageV2) appContext
  runInContext (insertPackage differentPackage) appContext
  return ()
