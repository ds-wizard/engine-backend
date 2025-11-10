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
import qualified Wizard.Database.Migration.Development.Common.CommonSchemaMigration as Common
import qualified Wizard.Database.Migration.Development.Document.DocumentMigration as Document
import qualified Wizard.Database.Migration.Development.Document.DocumentSchemaMigration as Document
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplate
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateSchemaMigration as DocumentTemplate
import qualified Wizard.Database.Migration.Development.Feedback.FeedbackMigration as Feedback
import qualified Wizard.Database.Migration.Development.Feedback.FeedbackSchemaMigration as Feedback
import qualified Wizard.Database.Migration.Development.Instance.InstanceSchemaMigration as Instance
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelCacheSchemaMigration as KnowledgeModelCache
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorSchemaMigration as KnowledgeModelEditor
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelMigrationMigration as KnowledgeModelMigrator
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelMigrationSchemaMigration as KnowledgeModelMigrator
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageSchemaMigration as KnowledgeModelPackage
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelSecretMigration as KnowledgeModelSecret
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelSecretSchemaMigration as KnowledgeModelSecret
import qualified Wizard.Database.Migration.Development.Locale.LocaleMigration as Locale
import qualified Wizard.Database.Migration.Development.Locale.LocaleSchemaMigration as Locale
import qualified Wizard.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration as PersistentCommand
import qualified Wizard.Database.Migration.Development.Questionnaire.MigratorMigration as QuestionnaireMigrator
import qualified Wizard.Database.Migration.Development.Questionnaire.MigratorSchemaMigration as QuestionnaireMigrator
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as Questionnaire
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireSchemaMigration as Questionnaire
import qualified Wizard.Database.Migration.Development.QuestionnaireAction.QuestionnaireActionMigration as QuestionnaireAction
import qualified Wizard.Database.Migration.Development.QuestionnaireAction.QuestionnaireActionSchemaMigration as QuestionnaireAction
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
import qualified WizardLib.Public.Database.Migration.Development.ExternalLink.ExternalLinkMigration as ExternalLink
import qualified WizardLib.Public.Database.Migration.Development.ExternalLink.ExternalLinkSchemaMigration as ExternalLink

runMigration = runAppContextWithBaseContext $ do
  logInfo _CMP_MIGRATION "started"
  -- 1. Drop DB triggers
  Document.dropTriggers
  Questionnaire.dropTriggers
  Locale.dropTriggers
  -- 2. Drop DB functions
  KnowledgeModelEditor.dropFunctions
  KnowledgeModelPackage.dropFunctions
  Common.dropFunctions
  -- 3. Drop DB schema
  ExternalLink.dropTables
  Component.dropTables
  Registry.dropTables
  QuestionnaireAction.dropTables
  QuestionnaireImporter.dropTables
  Audit.dropTables
  Prefab.dropTables
  PersistentCommand.dropTables
  Submission.dropTables
  ActionKey.dropTables
  Feedback.dropTables
  KnowledgeModelMigrator.dropTables
  KnowledgeModelEditor.dropTables
  KnowledgeModelCache.dropTables
  Document.dropTables
  QuestionnaireMigrator.dropTables
  Questionnaire.dropTables
  KnowledgeModelSecret.dropTables
  KnowledgeModelPackage.dropTables
  TemporaryFile.dropTables
  User.dropTables
  Tenant.dropConfigTables
  DocumentTemplate.dropTables
  Locale.dropTables
  Tenant.dropTables
  Instance.dropTables
  -- 4. Drop DB Types
  Common.dropTypes
  -- 5. Create DB Types
  Common.createTypes
  -- 6. Create schema
  Instance.createTables
  Tenant.createTables
  Locale.createTables
  DocumentTemplate.createTables
  Tenant.createConfigTables
  User.createTables
  TemporaryFile.createTables
  KnowledgeModelPackage.createTables
  KnowledgeModelSecret.createTables
  ActionKey.createTables
  Feedback.createTables
  KnowledgeModelEditor.createTables
  KnowledgeModelCache.createTables
  Questionnaire.createTables
  DocumentTemplate.createDraftDataTable
  Document.createTables
  QuestionnaireMigrator.createTables
  KnowledgeModelMigrator.createTables
  Submission.createTables
  PersistentCommand.createTables
  Prefab.createTables
  Audit.createTables
  QuestionnaireAction.createTables
  QuestionnaireImporter.createTables
  Registry.createTables
  Component.createTables
  ExternalLink.createTables
  -- 7. Create DB functions
  Common.createFunctions
  KnowledgeModelPackage.createFunctions
  KnowledgeModelEditor.createFunctions
  -- 8. Create missing foreign key constraints
  User.createUserLocaleForeignKeyConstraint
  -- 9. Create DB triggers
  Locale.createTriggers
  Questionnaire.createTriggers
  Document.createTriggers
  -- 10. Load S3 fixtures
  -- DocumentTemplate.runS3Migration
  -- Locale.runS3Migration
  -- 11. Load fixtures
  Tenant.runMigration
  User.runMigration
  KnowledgeModelPackage.runMigration
  KnowledgeModelSecret.runMigration
  DocumentTemplate.runMigration
  ActionKey.runMigration
  KnowledgeModelEditor.runMigration
  Questionnaire.runMigration
  Feedback.runMigration
  Document.runMigration
  QuestionnaireMigrator.runMigration
  KnowledgeModelMigrator.runMigration
  PersistentCommand.runMigration
  Prefab.runMigration
  Audit.runMigration
  QuestionnaireAction.runMigration
  QuestionnaireImporter.runMigration
  Registry.runMigration
  Locale.runMigration
  Component.runMigration
  ExternalLink.runMigration
  logInfo _CMP_MIGRATION "ended"
  return Nothing
