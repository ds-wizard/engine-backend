module Database.Migration.Development.Package.PackageMigration where

import Constant.Component
import Database.DAO.Package.PackageDAO
import Database.Migration.Development.Package.Data.Packages
import Util.Logger

runMigration = do
  logInfo $ msg _CMP_MIGRATION "(Package/Package) started"
  deletePackages
  insertPackage globalPackageEmpty
  insertPackage globalPackage
  insertPackage netherlandsPackage
  insertPackage netherlandsPackageV2
  logInfo $ msg _CMP_MIGRATION "(Package/Package) ended"
