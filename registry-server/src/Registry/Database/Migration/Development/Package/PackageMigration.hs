module Registry.Database.Migration.Development.Package.PackageMigration where

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Registry.Util.Logger
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Fixtures/Package) started"
  deletePackages
  insertPackage globalPackageEmpty
  insertPackage globalPackage
  insertPackage netherlandsPackage
  insertPackage netherlandsPackageV2
  logInfo _CMP_MIGRATION "(Fixtures/Package) ended"
