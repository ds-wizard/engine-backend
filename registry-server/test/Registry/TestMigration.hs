module Registry.TestMigration where

import Registry.Database.DAO.Audit.AuditEntryDAO
import Registry.Database.DAO.Organization.OrganizationDAO
import qualified Registry.Database.Migration.Development.ActionKey.ActionKeySchemaMigration as ActionKey
import qualified Registry.Database.Migration.Development.Audit.AuditSchemaMigration as Audit
import qualified Registry.Database.Migration.Development.DocumentTemplate.DocumentTemplateSchemaMigration as DocumentTemplate
import qualified Registry.Database.Migration.Development.Locale.LocaleSchemaMigration as Locale
import qualified Registry.Database.Migration.Development.Organization.OrganizationSchemaMigration as Organization
import qualified Registry.Database.Migration.Development.Package.PackageSchemaMigration as Package
import qualified Registry.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration as PersistentCommand
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import Shared.ActionKey.Database.DAO.ActionKey.ActionKeyDAO
import Shared.Component.Database.DAO.Component.ComponentDAO
import qualified Shared.Component.Database.Migration.Development.Component.ComponentSchemaMigration as Component
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
    runInContext Component.dropTables appContext
    runInContext Locale.dropTables appContext
    runInContext PersistentCommand.dropTables appContext
    runInContext ActionKey.dropTables appContext
    runInContext Audit.dropTables appContext
    runInContext Organization.dropTables appContext
    runInContext Package.dropTables appContext
    runInContext DocumentTemplate.dropTables appContext
    -- 2. Create
    putStrLn "DB: Creating schema"
    runInContext Organization.createTables appContext
    runInContext Package.createTables appContext
    runInContext ActionKey.createTables appContext
    runInContext Audit.createTables appContext
    runInContext DocumentTemplate.createTables appContext
    runInContext PersistentCommand.createTables appContext
    runInContext Locale.createTables appContext
    runInContext Component.createTables appContext

resetDB appContext = do
  runInContext deletePersistentCommands appContext
  runInContext deleteActionKeys appContext
  runInContext deleteAuditEntries appContext
  runInContext deletePackages appContext
  runInContext deleteDocumentTemplates appContext
  runInContext deleteLocales appContext
  runInContext deleteOrganizations appContext
  runInContext deleteComponents appContext
  runInContext (insertOrganization orgGlobal) appContext
  runInContext (insertOrganization orgNetherlands) appContext
  runInContext (insertPackage globalPackageEmpty) appContext
  runInContext (insertPackage globalPackage) appContext
  runInContext (insertPackage netherlandsPackage) appContext
  runInContext (insertPackage netherlandsPackageV2) appContext
  return ()
