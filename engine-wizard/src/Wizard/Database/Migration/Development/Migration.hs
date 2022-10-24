module Wizard.Database.Migration.Development.Migration
  ( runMigration
  ) where

import Shared.Constant.Component
import qualified Wizard.Database.Migration.Development.Acl.AclMigration as ACL
import qualified Wizard.Database.Migration.Development.Acl.AclSchemaMigration as ACL_Schema
import qualified Wizard.Database.Migration.Development.ActionKey.ActionKeyMigration as ACK
import qualified Wizard.Database.Migration.Development.ActionKey.ActionKeySchemaMigration as ACK_Schema
import qualified Wizard.Database.Migration.Development.App.AppMigration as A
import qualified Wizard.Database.Migration.Development.App.AppSchemaMigration as A_Schema
import qualified Wizard.Database.Migration.Development.Audit.AuditMigration as ADT
import qualified Wizard.Database.Migration.Development.Audit.AuditSchemaMigration as ADT_Schema
import qualified Wizard.Database.Migration.Development.BookReference.BookReferenceMigration as BR
import qualified Wizard.Database.Migration.Development.BookReference.BookReferenceSchemaMigration as BR_Schema
import qualified Wizard.Database.Migration.Development.Branch.BranchMigration as B
import qualified Wizard.Database.Migration.Development.Branch.BranchSchemaMigration as B_Schema
import qualified Wizard.Database.Migration.Development.Common.CommonSchemaMigration as CMN_Schema
import qualified Wizard.Database.Migration.Development.Config.AppConfigMigration as CFG
import qualified Wizard.Database.Migration.Development.Config.AppConfigSchemaMigration as CFG_Schema
import qualified Wizard.Database.Migration.Development.Document.DocumentMigration as DOC
import qualified Wizard.Database.Migration.Development.Document.DocumentSchemaMigration as DOC_Schema
import qualified Wizard.Database.Migration.Development.Feedback.FeedbackMigration as F
import qualified Wizard.Database.Migration.Development.Feedback.FeedbackSchemaMigration as F_Schema
import qualified Wizard.Database.Migration.Development.Limit.AppLimitMigration as AL
import qualified Wizard.Database.Migration.Development.Limit.AppLimitSchemaMigration as AL_Schema
import qualified Wizard.Database.Migration.Development.Migration.KnowledgeModel.MigratorMigration as KM_MIG
import qualified Wizard.Database.Migration.Development.Migration.KnowledgeModel.MigratorSchemaMigration as KM_MIG_Schema
import qualified Wizard.Database.Migration.Development.Migration.Questionnaire.MigratorMigration as QTN_MIG
import qualified Wizard.Database.Migration.Development.Migration.Questionnaire.MigratorSchemaMigration as QTN_MIG_Schema
import qualified Wizard.Database.Migration.Development.Package.PackageMigration as PKG
import qualified Wizard.Database.Migration.Development.Package.PackageSchemaMigration as PKG_Schema
import qualified Wizard.Database.Migration.Development.PersistentCommand.PersistentCommandMigration as PC
import qualified Wizard.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration as PC_Schema
import qualified Wizard.Database.Migration.Development.Plan.AppPlanMigration as AP
import qualified Wizard.Database.Migration.Development.Plan.AppPlanSchemaMigration as AP_Schema
import qualified Wizard.Database.Migration.Development.Prefab.PrefabMigration as PF
import qualified Wizard.Database.Migration.Development.Prefab.PrefabSchemaMigration as PF_Schema
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireSchemaMigration as QTN_Schema
import qualified Wizard.Database.Migration.Development.QuestionnaireImporter.QuestionnaireImporterMigration as QI
import qualified Wizard.Database.Migration.Development.QuestionnaireImporter.QuestionnaireImporterSchemaMigration as QI_Schema
import qualified Wizard.Database.Migration.Development.Registry.RegistryMigration as R
import qualified Wizard.Database.Migration.Development.Registry.RegistrySchemaMigration as R_Schema
import qualified Wizard.Database.Migration.Development.Submission.SubmissionSchemaMigration as SUB_Schema
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML
import qualified Wizard.Database.Migration.Development.Template.TemplateSchemaMigration as TML_Schema
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import qualified Wizard.Database.Migration.Development.User.UserSchemaMigration as U_Schema
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "started"
  -- 1. Drop DB functions
  B_Schema.dropFunctions
  PKG_Schema.dropFunctions
  TML_Schema.dropFunctions
  CMN_Schema.dropFunctions
  -- 2. Drop schema
  R_Schema.dropTables
  QI_Schema.dropTables
  ADT_Schema.dropTables
  PF_Schema.dropTables
  PC_Schema.dropTables
  SUB_Schema.dropTables
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
  PKG_Schema.dropFunctions
  ACL_Schema.dropTables
  U_Schema.dropTables
  CFG_Schema.dropTables
  AP_Schema.dropTables
  AL_Schema.dropTables
  A_Schema.dropTables
  -- 3. Create schema
  A_Schema.createTables
  AL_Schema.createTables
  AP_Schema.createTables
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
  SUB_Schema.createTables
  PC_Schema.createTables
  PF_Schema.createTables
  ADT_Schema.createTables
  QI_Schema.createTables
  R_Schema.createTables
  -- 4. Create DB functions
  CMN_Schema.createFunctions
  PKG_Schema.createFunctions
  TML_Schema.createFunctions
  B_Schema.createFunctions
  -- 5. Load S3 fixtures
  TML.runS3Migration
  -- 6. Load fixtures
  A.runMigration
  AL.runMigration
  AP.runMigration
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
  ACK.runMigration
  PC.runMigration
  PF.runMigration
  ADT.runMigration
  QI.runMigration
  R.runMigration
  logInfo _CMP_MIGRATION "ended"
  return Nothing
