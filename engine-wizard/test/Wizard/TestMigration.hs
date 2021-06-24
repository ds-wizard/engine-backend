module Wizard.TestMigration where

import Shared.Database.DAO.Package.PackageDAO
import Shared.Database.DAO.Template.TemplateDAO
import Shared.Database.Migration.Development.Package.Data.Packages
import Wizard.Database.DAO.ActionKey.ActionKeyDAO
import Wizard.Database.DAO.Branch.BranchDAO
import Wizard.Database.DAO.Config.AppConfigDAO
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Document.DocumentQueueDAO
import Wizard.Database.DAO.Feedback.FeedbackDAO
import qualified Wizard.Database.DAO.Migration.KnowledgeModel.MigratorDAO as KM_MigratorDAO
import qualified Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO as QTN_MigratorDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.User.UserDAO
import qualified Wizard.Database.Migration.Development.Acl.AclSchemaMigration as ACL_Schema
import qualified Wizard.Database.Migration.Development.ActionKey.ActionKeySchemaMigration as ACK_Schema
import qualified Wizard.Database.Migration.Development.BookReference.BookReferenceSchemaMigration as BR_Schema
import qualified Wizard.Database.Migration.Development.Branch.BranchSchemaMigration as B_Schema
import qualified Wizard.Database.Migration.Development.Config.AppConfigSchemaMigration as CFG_Schema
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import qualified Wizard.Database.Migration.Development.Document.DocumentSchemaMigration as DOC_Schema
import qualified Wizard.Database.Migration.Development.Feedback.FeedbackSchemaMigration as F_Schema
import qualified Wizard.Database.Migration.Development.Level.LevelSchemaMigration as LVL_Schema
import qualified Wizard.Database.Migration.Development.Migration.KnowledgeModel.MigratorSchemaMigration as KM_MIG_Schema
import qualified Wizard.Database.Migration.Development.Migration.Questionnaire.MigratorSchemaMigration as QTN_MIG_Schema
import qualified Wizard.Database.Migration.Development.Package.PackageSchemaMigration as PKG_Schema
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireSchemaMigration as QTN_Schema
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML
import qualified Wizard.Database.Migration.Development.Template.TemplateSchemaMigration as TML_Schema
import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Database.Migration.Development.User.UserSchemaMigration as U_Schema

import Wizard.Specs.Common

buildSchema appContext
    -- 1. Drop
 = do
  putStrLn "DB: dropping schema"
  runInContext ACK_Schema.dropTables appContext
  runInContext BR_Schema.dropTables appContext
  runInContext LVL_Schema.dropTables appContext
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
  -- 2. Create
  putStrLn "DB: Creating schema"
  runInContext U_Schema.createTables appContext
  runInContext ACL_Schema.createTables appContext
  runInContext TML_Schema.createTables appContext
  runInContext PKG_Schema.createTables appContext
  runInContext CFG_Schema.createTables appContext
  runInContext ACK_Schema.createTables appContext
  runInContext BR_Schema.createTables appContext
  runInContext LVL_Schema.createTables appContext
  runInContext F_Schema.createTables appContext
  runInContext B_Schema.createTables appContext
  runInContext QTN_Schema.createTables appContext
  runInContext DOC_Schema.createTables appContext
  runInContext QTN_MIG_Schema.createTables appContext
  runInContext KM_MIG_Schema.createTables appContext
  -- 3. Purge and put files into S3
  putStrLn "DB-S3: Purging and creating schema"
  runInContext TML.runS3Migration appContext

resetDB appContext = do
  runInContext deleteAppConfigs appContext
  runInContext (insertAppConfig defaultAppConfigEncrypted) appContext
  runInContext KM_MigratorDAO.deleteMigratorStates appContext
  runInContext QTN_MigratorDAO.deleteMigratorStates appContext
  runInContext deleteFeedbacks appContext
  runInContext deleteActionKeys appContext
  runInContext deleteBranches appContext
  runInContext deleteDocumentQueues appContext
  runInContext deleteDocuments appContext
  runInContext deleteQuestionnaires appContext
  runInContext deleteTemplates appContext
  runInContext deletePackages appContext
  runInContext deleteUsers appContext
  runInContext (insertUser userAlbert) appContext
  runInContext (insertPackage globalPackageEmpty) appContext
  runInContext (insertPackage globalPackage) appContext
  runInContext (insertPackage netherlandsPackage) appContext
  runInContext (insertPackage netherlandsPackageV2) appContext
  return ()
