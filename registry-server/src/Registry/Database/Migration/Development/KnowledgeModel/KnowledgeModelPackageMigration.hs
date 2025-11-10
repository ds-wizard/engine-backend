module Registry.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration where

import Data.Foldable (traverse_)

import Registry.Model.Context.AppContext
import Registry.Model.Context.ContextLenses ()
import Shared.Common.Util.Logger
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages

runMigration :: AppContextM ()
runMigration = do
  logInfo _CMP_MIGRATION "(Fixtures/KnowledgeModelPackage) started"
  deletePackages
  insertPackage globalKmPackageEmpty
  traverse_ insertPackageEvent globalKmPackageEmptyEvents
  insertPackage globalKmPackage
  traverse_ insertPackageEvent globalKmPackageEvents
  insertPackage netherlandsKmPackage
  traverse_ insertPackageEvent netherlandsKmPackageEvents
  insertPackage netherlandsKmPackageV2
  traverse_ insertPackageEvent netherlandsKmPackageV2Events
  logInfo _CMP_MIGRATION "(Fixtures/Package) ended"
