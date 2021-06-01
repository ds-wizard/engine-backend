module Registry.TestMigration where

import Registry.Database.DAO.ActionKey.ActionKeyDAO
import Registry.Database.DAO.Audit.AuditEntryDAO
import Registry.Database.DAO.Organization.OrganizationDAO
import qualified Registry.Database.Migration.Development.ActionKey.ActionKeySchemaMigration as ACK_Schema
import qualified Registry.Database.Migration.Development.Audit.AuditSchemaMigration as ADT_Schema
import Registry.Database.Migration.Development.Organization.Data.Organizations
import qualified Registry.Database.Migration.Development.Organization.OrganizationSchemaMigration as ORG_Schema
import qualified Registry.Database.Migration.Development.Package.PackageSchemaMigration as PKG_Schema
import qualified Registry.Database.Migration.Development.Template.TemplateSchemaMigration as TML_Schema
import Shared.Database.DAO.Package.PackageDAO
import Shared.Database.DAO.Template.TemplateDAO
import Shared.Database.Migration.Development.Package.Data.Packages

import Registry.Specs.Common

buildSchema appContext = do
  runInContext ACK_Schema.runMigration appContext
  runInContext ADT_Schema.runMigration appContext
  runInContext ORG_Schema.runMigration appContext
  runInContext PKG_Schema.runMigration appContext
  runInContext TML_Schema.runMigration appContext

resetDB appContext = do
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
