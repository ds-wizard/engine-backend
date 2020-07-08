module Wizard.Database.Migration.Development.Package.PackageMigration where

import Shared.Constant.Component
import Shared.Database.DAO.Package.PackageDAO
import Shared.Database.Migration.Development.Package.Data.Packages
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Package/Package) started"
  deletePackages
  insertPackage globalPackageEmpty
  insertPackage globalPackage
  insertPackage netherlandsPackage
  insertPackage netherlandsPackageV2
  logInfo _CMP_MIGRATION "(Package/Package) ended"
