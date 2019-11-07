module TestMigration where

import Database.DAO.ActionKey.ActionKeyDAO
import Database.DAO.Audit.AuditEntryDAO
import Database.DAO.Organization.OrganizationDAO
import Database.DAO.Package.PackageDAO
import Database.Migration.Development.Organization.Data.Organizations
import Database.Migration.Development.Package.Data.Packages

import Specs.Common

resetDB appContext = do
  runInContext (deleteOrganizations) appContext
  runInContext (insertOrganization orgDsw) appContext
  runInContext (insertOrganization orgNetherlands) appContext
  runInContext (deleteActionKeys) appContext
  runInContext (deleteAuditEntries) appContext
  runInContext (deletePackages) appContext
  runInContext (insertPackage globalPackageEmpty) appContext
  runInContext (insertPackage globalPackage) appContext
  runInContext (insertPackage netherlandsPackage) appContext
  runInContext (insertPackage netherlandsPackageV2) appContext
  return ()
