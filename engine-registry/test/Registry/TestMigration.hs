module Registry.TestMigration where

import Registry.Database.DAO.ActionKey.ActionKeyDAO
import Registry.Database.DAO.Audit.AuditEntryDAO
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Database.DAO.Package.PackageDAO
import Registry.Database.Migration.Development.Organization.Data.Organizations
import Shared.Database.Migration.Development.Package.Data.Packages

import Registry.Specs.Common

resetDB appContext = do
  runInContext (deleteOrganizations) appContext
  runInContext (insertOrganization orgGlobal) appContext
  runInContext (insertOrganization orgNetherlands) appContext
  runInContext (deleteActionKeys) appContext
  runInContext (deleteAuditEntries) appContext
  runInContext (deletePackages) appContext
  runInContext (insertPackage globalPackageEmpty) appContext
  runInContext (insertPackage globalPackage) appContext
  runInContext (insertPackage netherlandsPackage) appContext
  runInContext (insertPackage netherlandsPackageV2) appContext
  return ()
