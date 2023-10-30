module Wizard.Database.Migration.Development.Migration (
  runMigration,
) where

import qualified Shared.Audit.Database.Migration.Development.Audit.AuditMigration as ADT
import qualified Shared.Audit.Database.Migration.Development.Audit.AuditSchemaMigration as ADT_Schema
import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import qualified Shared.Component.Database.Migration.Development.Component.ComponentMigration as CMP
import qualified Shared.Component.Database.Migration.Development.Component.ComponentSchemaMigration as CMP_Schema
import qualified Shared.PersistentCommand.Database.Migration.Development.PersistentCommand.PersistentCommandMigration as PC
import qualified Shared.Prefab.Database.Migration.Development.Prefab.PrefabMigration as PF
import qualified Shared.Prefab.Database.Migration.Development.Prefab.PrefabSchemaMigration as PF_Schema
import qualified Wizard.Database.Migration.Development.ActionKey.ActionKeyMigration as ACK
import qualified Wizard.Database.Migration.Development.ActionKey.ActionKeySchemaMigration as ACK_Schema
import qualified Wizard.Database.Migration.Development.BookReference.BookReferenceMigration as BR
import qualified Wizard.Database.Migration.Development.BookReference.BookReferenceSchemaMigration as BR_Schema
import qualified Wizard.Database.Migration.Development.Branch.BranchMigration as B
import qualified Wizard.Database.Migration.Development.Branch.BranchSchemaMigration as B_Schema
import qualified Wizard.Database.Migration.Development.Common.CommonSchemaMigration as CMN_Schema
import qualified Wizard.Database.Migration.Development.Document.DocumentMigration as DOC
import qualified Wizard.Database.Migration.Development.Document.DocumentSchemaMigration as DOC_Schema
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateSchemaMigration as TML_Schema
import qualified Wizard.Database.Migration.Development.Feedback.FeedbackMigration as F
import qualified Wizard.Database.Migration.Development.Feedback.FeedbackSchemaMigration as F_Schema
import qualified Wizard.Database.Migration.Development.Instance.InstanceSchemaMigration as INS_Schema
import qualified Wizard.Database.Migration.Development.Locale.LocaleMigration as LOC
import qualified Wizard.Database.Migration.Development.Locale.LocaleSchemaMigration as LOC_Schema
import qualified Wizard.Database.Migration.Development.Migration.KnowledgeModel.MigratorMigration as KM_MIG
import qualified Wizard.Database.Migration.Development.Migration.KnowledgeModel.MigratorSchemaMigration as KM_MIG_Schema
import qualified Wizard.Database.Migration.Development.Migration.Questionnaire.MigratorMigration as QTN_MIG
import qualified Wizard.Database.Migration.Development.Migration.Questionnaire.MigratorSchemaMigration as QTN_MIG_Schema
import qualified Wizard.Database.Migration.Development.Package.PackageMigration as PKG
import qualified Wizard.Database.Migration.Development.Package.PackageSchemaMigration as PKG_Schema
import qualified Wizard.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration as PC_Schema
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireSchemaMigration as QTN_Schema
import qualified Wizard.Database.Migration.Development.QuestionnaireImporter.QuestionnaireImporterMigration as QI
import qualified Wizard.Database.Migration.Development.QuestionnaireImporter.QuestionnaireImporterSchemaMigration as QI_Schema
import qualified Wizard.Database.Migration.Development.Registry.RegistryMigration as R
import qualified Wizard.Database.Migration.Development.Registry.RegistrySchemaMigration as R_Schema
import qualified Wizard.Database.Migration.Development.Submission.SubmissionSchemaMigration as SUB_Schema
import qualified Wizard.Database.Migration.Development.TemporaryFile.TemporaryFileSchemaMigration as TF_Schema
import qualified Wizard.Database.Migration.Development.Tenant.TenantMigration as TNT
import qualified Wizard.Database.Migration.Development.Tenant.TenantSchemaMigration as TNT_Schema
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import qualified Wizard.Database.Migration.Development.User.UserSchemaMigration as U_Schema

runMigration = do
  logInfo _CMP_MIGRATION "started"
  -- 1. Drop DB functions
  B_Schema.dropFunctions
  PKG_Schema.dropFunctions
  TML_Schema.dropFunctions
  CMN_Schema.dropFunctions
  -- 2. Drop schema
  CMP_Schema.dropTables
  TF_Schema.dropTables
  LOC_Schema.dropTables
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
  U_Schema.dropTables
  TNT_Schema.dropTables
  INS_Schema.dropTables
  -- 3. Create schema
  INS_Schema.createTables
  TNT_Schema.createTables
  U_Schema.createTables
  TML_Schema.createTables
  PKG_Schema.createTables
  ACK_Schema.createTables
  BR_Schema.createTables
  F_Schema.createTables
  B_Schema.createTables
  QTN_Schema.createTables
  TML_Schema.createDraftDataTable
  DOC_Schema.createTables
  QTN_MIG_Schema.createTables
  KM_MIG_Schema.createTables
  SUB_Schema.createTables
  PC_Schema.createTables
  PF_Schema.createTables
  ADT_Schema.createTables
  QI_Schema.createTables
  R_Schema.createTables
  LOC_Schema.createTables
  TF_Schema.createTables
  CMP_Schema.createTables
  -- 4. Create DB functions
  CMN_Schema.createFunctions
  PKG_Schema.createFunctions
  TML_Schema.createFunctions
  B_Schema.createFunctions
  -- 5. Load S3 fixtures
  TML.runS3Migration
  LOC.runS3Migration
  -- 6. Load fixtures
  TNT.runMigration
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
  ACK.runMigration
  PC.runMigration
  PF.runMigration
  ADT.runMigration
  QI.runMigration
  R.runMigration
  LOC.runMigration
  CMP.runMigration
  logInfo _CMP_MIGRATION "ended"
  return Nothing
