module Wizard.Service.Migration.Metamodel.MigratorService
  ( migratePackageBundle
  , migrateCompleteDatabase
  ) where

import Control.Monad.Except (throwError)
import Data.Aeson
import Data.Bson.Generic
import qualified Data.Text as T
import Database.MongoDB ((=:), delete, find, insertMany, rest, select)

import Shared.Api.Resource.Package.PackageJM ()
import Shared.Constant.KnowledgeModel
import Wizard.Api.Resource.Branch.BranchWithEventsJM ()
import Wizard.Api.Resource.Migration.KnowledgeModel.MigratorStateDetailJM ()
import Wizard.Constant.Component
import Wizard.Database.BSON.Branch.BranchWithEvents ()
import Wizard.Database.BSON.Migration.KnowledgeModel.MigratorState ()
import Wizard.Database.BSON.Package.PackageWithEvents ()
import Wizard.Database.DAO.Common
import Wizard.Localization.Messages.Internal
import Wizard.Model.Context.AppContext
import qualified Wizard.Service.Branch.BranchMapper as BranchMapper
import qualified Wizard.Service.Migration.KnowledgeModel.MigratorMapper as KMMigratorMapper
import qualified Wizard.Service.Migration.Metamodel.Migrator.BranchMigrator as BranchMigrator
import qualified Wizard.Service.Migration.Metamodel.Migrator.KnowledgeModelMigrationMigrator as KMMMigrator
import qualified Wizard.Service.Migration.Metamodel.Migrator.PackageBundleMigrator as PBMigrator
import qualified Wizard.Service.Migration.Metamodel.Migrator.PackageMigrator as PackageMigrator
import qualified Wizard.Service.Package.PackageMapper as PackageMapper
import Wizard.Util.BSONtoJSON (mapBSONDocumentToJSONObject)
import Wizard.Util.List (foldEither)
import Wizard.Util.Logger

migratePackageBundle :: Value -> AppContextM Value
migratePackageBundle value =
  let eResult = PBMigrator.migrate value
   in case eResult of
        Right result -> return result
        Left error -> throwError error

migrateCompleteDatabase :: AppContextM ()
migrateCompleteDatabase = do
  migratePackages
  migrateBranches
  migrateKnowledgeModelMigrations

-- ---------------------------
-- PRIVATE
-- ---------------------------
migratePackages :: AppContextM ()
migratePackages = migrateOutdatedModels "packages" PackageMapper.fromDTO PackageMigrator.migrate

migrateBranches :: AppContextM ()
migrateBranches = migrateOutdatedModels "branches" BranchMapper.fromWithEventsDTO BranchMigrator.migrate

migrateKnowledgeModelMigrations :: AppContextM ()
migrateKnowledgeModelMigrations =
  migrateOutdatedModels "kmMigrations" KMMigratorMapper.fromDetailDTO KMMMigrator.migrate

findOutdatedModels collection =
  runDB $ rest =<< find (select ["metamodelVersion" =: ["$ne" =: kmMetamodelVersion]] (T.pack collection))

removeOutdatedModels collection =
  runDB . delete $ select ["metamodelVersion" =: ["$ne" =: kmMetamodelVersion]] (T.pack collection)

insertUpdatedModels collection models = runDB $ insertMany (T.pack collection) models

migrateOutdatedModels collection dtoMapper migrateFn = do
  logMigrationStarted collection
  entities <- findOutdatedModels collection
  case foldEither $ migrateFn . Object . mapBSONDocumentToJSONObject <$> entities of
    Right upgradedEntities -> do
      logMigrationMigrationApplied collection
      case eitherDecode . encode $ upgradedEntities of
        Right objects -> do
          documents <- convertToBSON objects
          removeOutdatedModels collection
          insertUpdatedModels collection documents
          logMigrationCompleted collection
          return ()
        Left error -> do
          logMigrationFailedToConvertToBson collection error
          return ()
    Left error -> do
      logMigrationFailedToMigrateCollection collection error
      return ()
  where
    convertToBSON objects = do
      logMigrationStartConvertingToBson collection
      let documents = toBSON <$> (dtoMapper <$> objects)
      logMigrationConvertedToBson collection
      return documents

-- --------------------------------
-- LOGGER
-- --------------------------------
logMigrationStarted collection = logInfo _CMP_SERVICE $ "Metamodel Migration ('" ++ collection ++ "'): started"

logMigrationMigrationApplied collection =
  logInfo _CMP_SERVICE $ "Metamodel Migration ('" ++ collection ++ "'): migration applied"

logMigrationStartConvertingToBson collection =
  logInfo _CMP_SERVICE $ "Metamodel Migration ('" ++ collection ++ "'): start converting to BSON"

logMigrationConvertedToBson collection =
  logInfo _CMP_SERVICE $ "Metamodel Migration ('" ++ collection ++ "'): converted to BSON"

logMigrationCompleted collection = logInfo _CMP_SERVICE $ "Metamodel Migration ('" ++ collection ++ "'): completed"

logMigrationNotApplied collection =
  logInfo _CMP_SERVICE $ "Metamodel Migration ('" ++ collection ++ "'): nothing to convert"

logMigrationFailedToConvertToBson collection error = do
  logError _CMP_SERVICE $ _ERROR_SERVICE_MIGRATION_METAMODEL__FAILED_CONVERT_TO_BSON collection
  logError _CMP_SERVICE . show $ error

logMigrationFailedToMigrateCollection collection error = do
  logError _CMP_SERVICE $ _ERROR_SERVICE_MIGRATION_METAMODEL__FAILED_TO_MIGRATE_COLLECTION collection
  logError _CMP_SERVICE . show $ error
