module Wizard.Service.KnowledgeModel.Metamodel.Migrator.KnowledgeModelPackageMigrator (
  migrateAll,
) where

import Control.Monad (void)
import Control.Monad.Reader (asks)
import qualified Data.Aeson as A
import Data.Foldable (traverse_)
import qualified Data.UUID as U
import qualified Data.Vector as Vector

import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelRawEventJM ()
import Shared.KnowledgeModel.Constant.KnowledgeModel
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelPackageDAO
import Wizard.Model.Context.AppContext
import Wizard.Service.KnowledgeModel.Metamodel.Migrator.CommonDB

migrateAll :: AppContextM ()
migrateAll = do
  logMigrationStarted "knowledge_model_package"
  kmPkgs <- findPackagesByUnsupportedMetamodelVersion kmMetamodelVersion
  tenantUuid <- asks currentTenantUuid
  traverse_ (migrateOneInDB tenantUuid) kmPkgs
  logMigrationCompleted "knowledge_model_package"

-- --------------------------------
-- PRIVATE
-- --------------------------------
migrateOneInDB :: U.UUID -> KnowledgeModelPackage -> AppContextM ()
migrateOneInDB tenantUuid pkg = do
  kmEvents <- findPackageRawEvents pkg.pId
  let events = A.Array . Vector.fromList . fmap (A.toJSON . toRawEvent) $ kmEvents
  migrateEventField "knowledge_model_package" pkg.createdAt pkg.metamodelVersion events $ \eventsMigratedValue -> do
    case A.fromJSON eventsMigratedValue of
      A.Error error -> logMigrationFailedToConvertToNewMetamodelVersion "knowledge_model_package" error
      A.Success eventsMigrated -> do
        let kmEventsMigrated = fmap (toPackageRawEvent pkg.pId tenantUuid) eventsMigrated
        deletePackageEventsById pkg.pId
        traverse_ insertPackageRawEvent kmEventsMigrated
        void $ updatePackageMetamodelVersion pkg.pId kmMetamodelVersion
