module Registry.Database.Migration.Development.Migration
  ( runMigration
  ) where

import qualified Registry.Database.Migration.Development.ActionKey.ActionKeySchemaMigration as ACK_Schema
import qualified Registry.Database.Migration.Development.Audit.AuditSchemaMigration as ADT_Schema
import qualified Registry.Database.Migration.Development.Organization.OrganizationMigration as ORG
import qualified Registry.Database.Migration.Development.Organization.OrganizationSchemaMigration as ORG_Schema
import qualified Registry.Database.Migration.Development.Package.PackageMigration as PKG
import qualified Registry.Database.Migration.Development.Package.PackageSchemaMigration as PKG_Schema
import qualified Registry.Database.Migration.Development.PersistentCommand.PersistentCommandMigration as PC
import qualified Registry.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration as PC_Schema
import qualified Registry.Database.Migration.Development.Template.TemplateMigration as TML
import qualified Registry.Database.Migration.Development.Template.TemplateSchemaMigration as TML_Schema
import Registry.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "started"
  -- 1. Drop schema
  PC_Schema.dropTables
  ORG_Schema.dropTables
  PKG_Schema.dropTables
  ACK_Schema.dropTables
  ADT_Schema.dropTables
  TML_Schema.dropTables
  -- 2. Create schema
  ORG_Schema.createTables
  PKG_Schema.createTables
  ACK_Schema.createTables
  ADT_Schema.createTables
  TML_Schema.createTables
  PC_Schema.createTables
  -- 3. Load fixtures
  ORG.runMigration
  PKG.runMigration
  TML.runMigration
  PC.runMigration
  logInfo _CMP_MIGRATION "ended"
  return Nothing
