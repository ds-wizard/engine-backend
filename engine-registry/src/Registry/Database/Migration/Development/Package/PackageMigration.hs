module Registry.Database.Migration.Development.Package.PackageMigration where

import Registry.Constant.Component
import Registry.Database.DAO.Package.PackageDAO
import Registry.Util.Logger
import Shared.Database.Migration.Development.Package.Data.Packages

runMigration = do
  logInfo _CMP_MIGRATION "(Package/Package) started"
  deletePackages
  insertPackage globalPackageEmpty
  insertPackage globalPackage
  insertPackage netherlandsPackage
  insertPackage netherlandsPackageV2
  logInfo _CMP_MIGRATION "(Package/Package) ended"
