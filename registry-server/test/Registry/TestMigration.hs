module Registry.TestMigration where

import Registry.Database.DAO.Audit.AuditEntryDAO
import Registry.Database.DAO.Organization.OrganizationDAO
import qualified Registry.Database.Migration.Development.ActionKey.ActionKeySchemaMigration as ACK_Schema
import qualified Registry.Database.Migration.Development.Audit.AuditSchemaMigration as ADT_Schema
import qualified Registry.Database.Migration.Development.DocumentTemplate.DocumentTemplateSchemaMigration as TML_Schema
import qualified Registry.Database.Migration.Development.Locale.LocaleSchemaMigration as LOC_Schema
import Registry.Database.Migration.Development.Organization.Data.Organizations
import qualified Registry.Database.Migration.Development.Organization.OrganizationSchemaMigration as ORG_Schema
import qualified Registry.Database.Migration.Development.Package.PackageSchemaMigration as PKG_Schema
import qualified Registry.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration as PC_Schema
import Shared.ActionKey.Database.DAO.ActionKey.ActionKeyDAO
import Shared.Common.Database.DAO.Component.ComponentDAO
import qualified Shared.Common.Database.Migration.Development.Component.ComponentSchemaMigration as CMP_Schema
import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages

import Registry.Specs.Common

buildSchema appContext =
  -- 1. Drop
  do
    putStrLn "DB: dropping schema"
    runInContext CMP_Schema.dropTables appContext
    runInContext LOC_Schema.dropTables appContext
    runInContext PC_Schema.dropTables appContext
    runInContext ACK_Schema.dropTables appContext
    runInContext ADT_Schema.dropTables appContext
    runInContext ORG_Schema.dropTables appContext
    runInContext PKG_Schema.dropTables appContext
    runInContext TML_Schema.dropTables appContext
    -- 2. Create
    putStrLn "DB: Creating schema"
    runInContext ORG_Schema.createTables appContext
    runInContext PKG_Schema.createTables appContext
    runInContext ACK_Schema.createTables appContext
    runInContext ADT_Schema.createTables appContext
    runInContext TML_Schema.createTables appContext
    runInContext PC_Schema.createTables appContext
    runInContext LOC_Schema.createTables appContext
    runInContext CMP_Schema.createTables appContext

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
  runInContext deleteDocumentTemplates appContext
  runInContext deleteLocales appContext
  runInContext deleteComponents appContext
  return ()
