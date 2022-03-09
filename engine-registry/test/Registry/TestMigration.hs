module Registry.TestMigration where

import Registry.Database.DAO.ActionKey.ActionKeyDAO
import Registry.Database.DAO.Audit.AuditEntryDAO
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Database.DAO.PersistentCommand.PersistentCommandDAO
import qualified Registry.Database.Migration.Development.ActionKey.ActionKeySchemaMigration as ACK_Schema
import qualified Registry.Database.Migration.Development.Audit.AuditSchemaMigration as ADT_Schema
import Registry.Database.Migration.Development.Organization.Data.Organizations
import qualified Registry.Database.Migration.Development.Organization.OrganizationSchemaMigration as ORG_Schema
import qualified Registry.Database.Migration.Development.Package.PackageSchemaMigration as PKG_Schema
import qualified Registry.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration as PC_Schema
import qualified Registry.Database.Migration.Development.Template.TemplateSchemaMigration as TML_Schema
import Shared.Database.DAO.Package.PackageDAO
import Shared.Database.DAO.Template.TemplateDAO
import Shared.Database.Migration.Development.Package.Data.Packages

import Registry.Specs.Common

buildSchema appContext
  -- 1. Drop
 = do
  putStrLn "DB: dropping schema"
  runInContext PC_Schema.dropTables appContext
  runInContext ACK_Schema.dropTables appContext
  runInContext ADT_Schema.dropTables appContext
  runInContext ORG_Schema.dropTables appContext
  runInContext PKG_Schema.dropTables appContext
  runInContext TML_Schema.dropTables appContext
  -- 2. Create
  putStrLn "DB: Creating schema"
  runInContext ACK_Schema.createTables appContext
  runInContext ADT_Schema.createTables appContext
  runInContext ORG_Schema.createTables appContext
  runInContext PKG_Schema.createTables appContext
  runInContext TML_Schema.createTables appContext
  runInContext PC_Schema.createTables appContext

resetDB appContext = do
  runInContext deletePersistentCommands appContext
  runInContext deleteOrganizations appContext
  runInContext (insertOrganization orgGlobal) appContext
  runInContext (insertOrganization orgNetherlands) appContext
  runInContext deleteActionKeys appContext
  runInContext deleteAuditEntries appContext
  runInContext deletePackages appContext
  runInContext (insertPackage globalPackageEmpty) appContext
  runInContext (insertPackage globalPackage) appContext
  runInContext (insertPackage netherlandsPackage) appContext
  runInContext (insertPackage netherlandsPackageV2) appContext
  runInContext deleteTemplates appContext
  return ()
