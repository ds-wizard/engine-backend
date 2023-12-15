module Wizard.Database.Migration.Development.Migration (
  runMigration,
) where

import qualified Shared.Audit.Database.Migration.Development.Audit.AuditMigration as Audit
import qualified Shared.Audit.Database.Migration.Development.Audit.AuditSchemaMigration as Audit
import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import qualified Shared.Component.Database.Migration.Development.Component.ComponentMigration as Component
import qualified Shared.Component.Database.Migration.Development.Component.ComponentSchemaMigration as Component
import qualified Shared.PersistentCommand.Database.Migration.Development.PersistentCommand.PersistentCommandMigration as PersistentCommand
import qualified Shared.Prefab.Database.Migration.Development.Prefab.PrefabMigration as Prefab
import qualified Shared.Prefab.Database.Migration.Development.Prefab.PrefabSchemaMigration as Prefab
import qualified Wizard.Database.Migration.Development.ActionKey.ActionKeyMigration as ActionKey
import qualified Wizard.Database.Migration.Development.ActionKey.ActionKeySchemaMigration as ActionKey
import qualified Wizard.Database.Migration.Development.BookReference.BookReferenceMigration as BookReference
import qualified Wizard.Database.Migration.Development.BookReference.BookReferenceSchemaMigration as BookReference
import qualified Wizard.Database.Migration.Development.Branch.BranchMigration as Branch
import qualified Wizard.Database.Migration.Development.Branch.BranchSchemaMigration as Branch
import qualified Wizard.Database.Migration.Development.Common.CommonSchemaMigration as Common
import qualified Wizard.Database.Migration.Development.Document.DocumentMigration as Document
import qualified Wizard.Database.Migration.Development.Document.DocumentSchemaMigration as Document
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplate
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateSchemaMigration as DocumentTemplate
import qualified Wizard.Database.Migration.Development.Feedback.FeedbackMigration as Feedback
import qualified Wizard.Database.Migration.Development.Feedback.FeedbackSchemaMigration as Feedback
import qualified Wizard.Database.Migration.Development.Instance.InstanceSchemaMigration as Instance
import qualified Wizard.Database.Migration.Development.Locale.LocaleMigration as Locale
import qualified Wizard.Database.Migration.Development.Locale.LocaleSchemaMigration as Locale
import qualified Wizard.Database.Migration.Development.Migration.KnowledgeModel.MigratorMigration as KnowledgeModelMigrator
import qualified Wizard.Database.Migration.Development.Migration.KnowledgeModel.MigratorSchemaMigration as KnowledgeModelMigrator
import qualified Wizard.Database.Migration.Development.Migration.Questionnaire.MigratorMigration as QuestionnaireMigrator
import qualified Wizard.Database.Migration.Development.Migration.Questionnaire.MigratorSchemaMigration as QuestionnaireMigrator
import qualified Wizard.Database.Migration.Development.Package.PackageMigration as Package
import qualified Wizard.Database.Migration.Development.Package.PackageSchemaMigration as Package
import qualified Wizard.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration as PersistentCommand
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as Questionnaire
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireSchemaMigration as Questionnaire
import qualified Wizard.Database.Migration.Development.QuestionnaireImporter.QuestionnaireImporterMigration as QuestionnaireImporter
import qualified Wizard.Database.Migration.Development.QuestionnaireImporter.QuestionnaireImporterSchemaMigration as QuestionnaireImporter
import qualified Wizard.Database.Migration.Development.Registry.RegistryMigration as Registry
import qualified Wizard.Database.Migration.Development.Registry.RegistrySchemaMigration as Registry
import qualified Wizard.Database.Migration.Development.Submission.SubmissionSchemaMigration as Submission
import qualified Wizard.Database.Migration.Development.TemporaryFile.TemporaryFileSchemaMigration as TemporaryFile
import qualified Wizard.Database.Migration.Development.Tenant.TenantMigration as Tenant
import qualified Wizard.Database.Migration.Development.Tenant.TenantSchemaMigration as Tenant
import qualified Wizard.Database.Migration.Development.User.UserMigration as User
import qualified Wizard.Database.Migration.Development.User.UserSchemaMigration as User
import Wizard.Model.Context.ContextMappers

runMigration = runAppContextWithBaseContext $ do
  logInfo _CMP_MIGRATION "started"
  -- 1. Drop DB functions
  Branch.dropFunctions
  DocumentTemplate.dropFunctions
  Package.dropFunctions
  Common.dropFunctions
  -- 2. Drop schema
  Component.dropTables
  TemporaryFile.dropTables
  Locale.dropTables
  Registry.dropTables
  QuestionnaireImporter.dropTables
  Audit.dropTables
  Prefab.dropTables
  PersistentCommand.dropTables
  Submission.dropTables
  KnowledgeModelMigrator.dropTables
  QuestionnaireMigrator.dropTables
  Document.dropTables
  Questionnaire.dropTables
  Branch.dropTables
  Feedback.dropTables
  BookReference.dropTables
  ActionKey.dropTables
  DocumentTemplate.dropTables
  Package.dropTables
  User.dropTables
  Tenant.dropTables
  Instance.dropTables
  -- 3. Create schema
  Instance.createTables
  Tenant.createTables
  User.createTables
  Package.createTables
  DocumentTemplate.createTables
  ActionKey.createTables
  BookReference.createTables
  Feedback.createTables
  Branch.createTables
  Questionnaire.createTables
  DocumentTemplate.createDraftDataTable
  Document.createTables
  QuestionnaireMigrator.createTables
  KnowledgeModelMigrator.createTables
  Submission.createTables
  PersistentCommand.createTables
  Prefab.createTables
  Audit.createTables
  QuestionnaireImporter.createTables
  Registry.createTables
  Locale.createTables
  TemporaryFile.createTables
  Component.createTables
  -- 4. Create DB functions
  Common.createFunctions
  Package.createFunctions
  DocumentTemplate.createFunctions
  Branch.createFunctions
  -- 5. Load S3 fixtures
  DocumentTemplate.runS3Migration
  Locale.runS3Migration
  -- 6. Load fixtures
  Tenant.runMigration
  User.runMigration
  Package.runMigration
  DocumentTemplate.runMigration
  ActionKey.runMigration
  BookReference.runMigration
  Feedback.runMigration
  Branch.runMigration
  Questionnaire.runMigration
  Document.runMigration
  QuestionnaireMigrator.runMigration
  KnowledgeModelMigrator.runMigration
  PersistentCommand.runMigration
  Prefab.runMigration
  Audit.runMigration
  QuestionnaireImporter.runMigration
  Registry.runMigration
  Locale.runMigration
  Component.runMigration
  logInfo _CMP_MIGRATION "ended"
  return Nothing
