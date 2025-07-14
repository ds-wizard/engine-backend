module Registry.Database.Migration.Development.Migration (
  runMigration,
) where

import qualified Registry.Database.Migration.Development.ActionKey.ActionKeySchemaMigration as ActionKey
import qualified Registry.Database.Migration.Development.Audit.AuditSchemaMigration as Audit
import qualified Registry.Database.Migration.Development.Common.CommonSchemaMigration as Common
import qualified Registry.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DocumentTemplate
import qualified Registry.Database.Migration.Development.DocumentTemplate.DocumentTemplateSchemaMigration as DocumentTemplate
import qualified Registry.Database.Migration.Development.Locale.LocaleMigration as Locale
import qualified Registry.Database.Migration.Development.Locale.LocaleSchemaMigration as Locale
import qualified Registry.Database.Migration.Development.Organization.OrganizationMigration as Organization
import qualified Registry.Database.Migration.Development.Organization.OrganizationSchemaMigration as Organization
import qualified Registry.Database.Migration.Development.Package.PackageMigration as Package
import qualified Registry.Database.Migration.Development.Package.PackageSchemaMigration as Package
import qualified Registry.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration as PersistentCommand
import Registry.Model.Context.ContextMappers
import Shared.Common.Util.Logger
import qualified Shared.Component.Database.Migration.Development.Component.ComponentMigration as Component
import qualified Shared.Component.Database.Migration.Development.Component.ComponentSchemaMigration as Component
import qualified Shared.PersistentCommand.Database.Migration.Development.PersistentCommand.PersistentCommandMigration as PersistentCommand

runMigration = runAppContextWithBaseContext $ do
  logInfo _CMP_MIGRATION "started"
  -- 1. Drop DB functions
  Common.dropFunctions
  -- 2. Drop schema
  Component.dropTables
  Locale.dropTables
  PersistentCommand.dropTables
  DocumentTemplate.dropTables
  Audit.dropTables
  ActionKey.dropTables
  Package.dropTables
  Organization.dropTables
  -- 3. Drop DB types
  Common.dropTypes
  -- 4. Create DB types
  Common.createTypes
  -- 5. Create schema
  Organization.createTables
  Package.createTables
  ActionKey.createTables
  Audit.createTables
  DocumentTemplate.createTables
  PersistentCommand.createTables
  Locale.createTables
  Component.createTables
  -- 6. Create DB functions
  Common.createFunctions
  -- 7. Load fixtures
  Organization.runMigration
  Package.runMigration
  DocumentTemplate.runMigration
  PersistentCommand.runMigration
  Locale.runMigration
  Component.runMigration
  logInfo _CMP_MIGRATION "ended"
  return Nothing
