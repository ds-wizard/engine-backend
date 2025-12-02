module Registry.TestMigration where

import Data.Foldable (traverse_)

import Registry.Database.DAO.Audit.AuditEntryDAO
import Registry.Database.DAO.Organization.OrganizationDAO
import qualified Registry.Database.Migration.Development.ActionKey.ActionKeySchemaMigration as ActionKey
import qualified Registry.Database.Migration.Development.Audit.AuditSchemaMigration as Audit
import qualified Registry.Database.Migration.Development.Common.CommonSchemaMigration as Common
import qualified Registry.Database.Migration.Development.DocumentTemplate.DocumentTemplateSchemaMigration as DocumentTemplate
import qualified Registry.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageSchemaMigration as KnowledgeModelPackage
import qualified Registry.Database.Migration.Development.Locale.LocaleSchemaMigration as Locale
import qualified Registry.Database.Migration.Development.Organization.OrganizationSchemaMigration as Organization
import qualified Registry.Database.Migration.Development.PersistentCommand.PersistentCommandSchemaMigration as PersistentCommand
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import Shared.ActionKey.Database.DAO.ActionKey.ActionKeyDAO
import Shared.Component.Database.DAO.Component.ComponentDAO
import qualified Shared.Component.Database.Migration.Development.Component.ComponentSchemaMigration as Component
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.Locale.Database.DAO.Locale.LocaleDAO
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO

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
    runInContext KnowledgeModelPackage.dropTables appContext
    runInContext DocumentTemplate.dropTables appContext
    putStrLn "DB: Drop DB types"
    runInContext Common.dropTypes appContext
    -- 2. Create
    putStrLn "DB: Create DB types"
    runInContext Common.createTypes appContext
    putStrLn "DB: Creating schema"
    runInContext Organization.createTables appContext
    runInContext KnowledgeModelPackage.createTables appContext
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
  runInContext (insertPackage globalKmPackageEmpty) appContext
  runInContext (traverse_ insertPackageEvent globalKmPackageEmptyEvents) appContext
  runInContext (insertPackage globalKmPackage) appContext
  runInContext (traverse_ insertPackageEvent globalKmPackageEvents) appContext
  runInContext (insertPackage netherlandsKmPackage) appContext
  runInContext (traverse_ insertPackageEvent netherlandsKmPackageEvents) appContext
  runInContext (insertPackage netherlandsKmPackageV2) appContext
  runInContext (traverse_ insertPackageEvent netherlandsKmPackageV2Events) appContext
  return ()
