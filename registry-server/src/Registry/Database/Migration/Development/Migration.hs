module Registry.Database.Migration.Development.Migration (
  runMigration,
) where

import qualified Registry.Database.Migration.Development.ActionKey.ActionKeySchemaMigration as ACK_Schema
import qualified Registry.Database.Migration.Development.Audit.AuditSchemaMigration as ADT_Schema
import qualified Registry.Database.Migration.Development.Common.CommonSchemaMigration as CMN_Schema
import qualified Registry.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import qualified Registry.Database.Migration.Development.DocumentTemplate.DocumentTemplateSchemaMigration as TML_Schema
import qualified Registry.Database.Migration.Development.Locale.LocaleMigration as LOC
import qualified Registry.Database.Migration.Development.Locale.LocaleSchemaMigration as LOC_Schema
import qualified Registry.Database.Migration.Development.Organization.OrganizationMigration as ORG
import qualified Registry.Database.Migration.Development.Organization.OrganizationSchemaMigration as ORG_Schema
import qualified Registry.Database.Migration.Development.Package.PackageMigration as PKG
import qualified Registry.Database.Migration.Development.Package.PackageSchemaMigration as PKG_Schema
import qualified Registry.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration as PC_Schema
import Registry.Util.Logger
import qualified Shared.Common.Database.Migration.Development.Component.ComponentMigration as CMP
import qualified Shared.Common.Database.Migration.Development.Component.ComponentSchemaMigration as CMP_Schema
import qualified Shared.PersistentCommand.Database.Migration.Development.PersistentCommand.PersistentCommandMigration as PC

runMigration = do
  logInfo _CMP_MIGRATION "started"
  -- 1. Drop DB functions
  CMN_Schema.dropFunctions
  -- 2. Drop schema
  CMP_Schema.dropTables
  LOC_Schema.dropTables
  PC_Schema.dropTables
  TML_Schema.dropTables
  ADT_Schema.dropTables
  ACK_Schema.dropTables
  PKG_Schema.dropTables
  ORG_Schema.dropTables
  -- 3. Create schema
  ORG_Schema.createTables
  PKG_Schema.createTables
  ACK_Schema.createTables
  ADT_Schema.createTables
  TML_Schema.createTables
  PC_Schema.createTables
  LOC_Schema.createTables
  CMP_Schema.createTables
  -- 4. Create DB functions
  CMN_Schema.createFunctions
  -- 5. Load fixtures
  ORG.runMigration
  PKG.runMigration
  TML.runMigration
  PC.runMigration
  LOC.runMigration
  CMP.runMigration
  logInfo _CMP_MIGRATION "ended"
  return Nothing
