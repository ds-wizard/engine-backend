module Wizard.Database.Migration.Development.Package.PackageMigration where

import Shared.Database.Migration.Development.Package.Data.Packages
import Wizard.Constant.Component
import Wizard.Database.DAO.Package.PackageDAO
import Wizard.Util.Logger

runMigration = do
  logInfo _CMP_MIGRATION "(Package/Package) started"
  deletePackages
  insertPackage globalPackageEmpty
  insertPackage globalPackage
  insertPackage netherlandsPackage
  insertPackage netherlandsPackageV2
  logInfo _CMP_MIGRATION "(Package/Package) ended"
