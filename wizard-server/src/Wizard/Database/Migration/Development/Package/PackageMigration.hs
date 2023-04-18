module Wizard.Database.Migration.Development.Package.PackageMigration where

import Shared.Common.Constant.Component
import Wizard.Database.Migration.Development.Package.Data.Packages
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Package/Package) started"
  deletePackages
  insertPackage globalPackageEmpty
  insertPackage globalPackage
  insertPackage netherlandsPackage
  insertPackage netherlandsPackageV2
  insertPackage differentPackage
  logInfo _CMP_MIGRATION "(Package/Package) ended"
