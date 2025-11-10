module Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration where

import Data.Foldable (traverse_)
import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Package/Package) started"
  deletePackages
  insertPackage globalKmPackageEmpty
  traverse_ insertPackageEvent globalKmPackageEmptyEvents
  insertPackage globalKmPackage
  traverse_ insertPackageEvent globalKmPackageEvents
  insertPackage netherlandsKmPackage
  traverse_ insertPackageEvent netherlandsKmPackageEvents
  insertPackage netherlandsKmPackageV2
  traverse_ insertPackageEvent netherlandsKmPackageV2Events
  insertPackage differentPackage
  traverse_ insertPackageEvent differentPackageEvents
  logInfo _CMP_MIGRATION "(Package/Package) ended"
