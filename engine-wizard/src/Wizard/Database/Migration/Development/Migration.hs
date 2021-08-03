module Wizard.Database.Migration.Development.Migration
  ( runMigration
  ) where

import Shared.Constant.Component
import qualified Wizard.Database.Migration.Development.Acl.AclMigration as ACL
import qualified Wizard.Database.Migration.Development.Acl.AclSchemaMigration as ACL_Schema
import qualified Wizard.Database.Migration.Development.ActionKey.ActionKeySchemaMigration as ACK_Schema
import qualified Wizard.Database.Migration.Development.BookReference.BookReferenceMigration as BR
import qualified Wizard.Database.Migration.Development.BookReference.BookReferenceSchemaMigration as BR_Schema
import qualified Wizard.Database.Migration.Development.Branch.BranchMigration as B
import qualified Wizard.Database.Migration.Development.Branch.BranchSchemaMigration as B_Schema
import qualified Wizard.Database.Migration.Development.Config.AppConfigMigration as CFG
import qualified Wizard.Database.Migration.Development.Config.AppConfigSchemaMigration as CFG_Schema
import qualified Wizard.Database.Migration.Development.Document.DocumentMigration as DOC
import qualified Wizard.Database.Migration.Development.Document.DocumentSchemaMigration as DOC_Schema
import qualified Wizard.Database.Migration.Development.Feedback.FeedbackMigration as F
import qualified Wizard.Database.Migration.Development.Feedback.FeedbackSchemaMigration as F_Schema
import qualified Wizard.Database.Migration.Development.Migration.KnowledgeModel.MigratorMigration as KM_MIG
import qualified Wizard.Database.Migration.Development.Migration.KnowledgeModel.MigratorSchemaMigration as KM_MIG_Schema
import qualified Wizard.Database.Migration.Development.Migration.Questionnaire.MigratorMigration as QTN_MIG
import qualified Wizard.Database.Migration.Development.Migration.Questionnaire.MigratorSchemaMigration as QTN_MIG_Schema
import qualified Wizard.Database.Migration.Development.Package.PackageMigration as PKG
import qualified Wizard.Database.Migration.Development.Package.PackageSchemaMigration as PKG_Schema
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireSchemaMigration as QTN_Schema
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML
import qualified Wizard.Database.Migration.Development.Template.TemplateSchemaMigration as TML_Schema
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import qualified Wizard.Database.Migration.Development.User.UserSchemaMigration as U_Schema
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "started"
  -- 1. Drop schema
  ACK_Schema.dropTables
  BR_Schema.dropTables
  F_Schema.dropTables
  KM_MIG_Schema.dropTables
  B_Schema.dropTables
  DOC_Schema.dropTables
  QTN_MIG_Schema.dropTables
  QTN_Schema.dropTables
  TML_Schema.dropTables
  PKG_Schema.dropTables
  ACL_Schema.dropTables
  U_Schema.dropTables
  CFG_Schema.dropTables
  -- 2. Create schema
  U_Schema.createTables
  ACL_Schema.createTables
  TML_Schema.createTables
  PKG_Schema.createTables
  CFG_Schema.createTables
  ACK_Schema.createTables
  BR_Schema.createTables
  F_Schema.createTables
  B_Schema.createTables
  QTN_Schema.createTables
  DOC_Schema.createTables
  QTN_MIG_Schema.createTables
  KM_MIG_Schema.createTables
  -- 3. Load S3 fixtures
  TML.runS3Migration
  -- 4. Load fixtures
  CFG.runMigration
  U.runMigration
  TML.runMigration
  PKG.runMigration
  B.runMigration
  KM_MIG.runMigration
  QTN.runMigration
  QTN_MIG.runMigration
  BR.runMigration
  F.runMigration
  DOC.runMigration
  ACL.runMigration
  logInfo _CMP_MIGRATION "ended"
