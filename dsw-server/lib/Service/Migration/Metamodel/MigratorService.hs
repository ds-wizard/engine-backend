module Service.Migration.Metamodel.MigratorService
  ( migratePackageBundle
  , migrateCompleteDatabase
  -- Helpers
  , heMigratePackageBundle
  ) where

import Data.Aeson
import Data.Bson.Generic
import qualified Data.Text as T
import Database.MongoDB
       ((=:), delete, find, findOne, insert, insertMany, rest, select)

import Api.Resource.Branch.BranchWithEventsJM ()
import Api.Resource.Migration.KnowledgeModel.MigratorStateDetailJM
       ()
import Api.Resource.Package.PackageJM ()
import Constant.Component
import Constant.KnowledgeModel
import Database.BSON.Branch.BranchWithEvents ()
import Database.BSON.Migration.KnowledgeModel.MigratorState ()
import Database.BSON.Package.PackageWithEvents ()
import Database.DAO.Common
import Localization
import Model.Context.AppContext
import Model.Error.Error
import qualified Service.Branch.BranchMapper as BranchMapper
import qualified Service.Migration.KnowledgeModel.MigratorMapper
       as KMMigratorMapper
import qualified
       Service.Migration.Metamodel.Migrator.BranchMigrator
       as BranchMigrator
import qualified
       Service.Migration.Metamodel.Migrator.KnowledgeModelMigrationMigrator
       as KMMMigrator
import qualified
       Service.Migration.Metamodel.Migrator.PackageBundleMigrator
       as PBMigrator
import qualified
       Service.Migration.Metamodel.Migrator.PackageMigrator
       as PackageMigrator
import qualified Service.Package.PackageMapper as PackageMapper
import Util.BSONtoJSON (mapBSONDocumentToJSONObject)
import Util.List (foldEither)
import Util.Logger (logError, logInfo, msg)

migratePackageBundle :: Value -> AppContextM (Either AppError Value)
migratePackageBundle = return . PBMigrator.migrate

migrateCompleteDatabase :: AppContextM ()
migrateCompleteDatabase = do
  migratePackages
  migrateBranches
  migrateKnowledgeModelMigrations
  migratePublicPackages

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

migratePublicPackages :: AppContextM ()
migratePublicPackages = do
  logMigrationStarted "publicPackages"
  mPackage <- runDB $ findOne (select [] "publicPackages")
  case mPackage of
    Just package ->
      case PackageMigrator.migrate . Object . mapBSONDocumentToJSONObject $ package of
        Right upgradedEntity -> do
          logMigrationMigrationApplied "publicPackages"
          case eitherDecode . encode $ upgradedEntity of
            Right object -> do
              document <- convertToBSON object
              runDB . delete $ select [] "publicPackages"
              runDB $ insert "publicPackages" document
              logMigrationCompleted "publicPackages"
              return ()
            Left error -> do
              logMigrationFailedToConvertToBson "publicPackages" error
              return ()
        Left error -> do
          logMigrationFailedToMigrateCollection "publicPackages" error
          return ()
    Nothing -> do
      logMigrationNotApplied "publicPackages"
      return ()
  where
    convertToBSON object = do
      logMigrationStartConvertingToBson "publicPackages"
      let documents = toBSON . PackageMapper.fromDTO $ object
      logMigrationConvertedToBson "publicPackages"
      return documents

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

-- ---------------------------
-- HELPERS
-- ---------------------------
heMigratePackageBundle encodedPb callback = do
  eitherResult <- migratePackageBundle encodedPb
  case eitherResult of
    Right result -> callback result
    Left error -> return . Left $ error

-- --------------------------------
-- LOGGER
-- --------------------------------
logMigrationStarted collection = logInfo . msg _CMP_SERVICE $ "Metamodel Migration ('" ++ collection ++ "'): started"

logMigrationMigrationApplied collection =
  logInfo . msg _CMP_SERVICE $ "Metamodel Migration ('" ++ collection ++ "'): migration applied"

logMigrationStartConvertingToBson collection =
  logInfo . msg _CMP_SERVICE $ "Metamodel Migration ('" ++ collection ++ "'): start converting to BSON"

logMigrationConvertedToBson collection =
  logInfo . msg _CMP_SERVICE $ "Metamodel Migration ('" ++ collection ++ "'): converted to BSON"

logMigrationCompleted collection =
  logInfo . msg _CMP_SERVICE $ "Metamodel Migration ('" ++ collection ++ "'): completed"

logMigrationNotApplied collection =
  logInfo . msg _CMP_SERVICE $ "Metamodel Migration ('" ++ collection ++ "'): nothing to convert"

logMigrationFailedToConvertToBson collection error = do
  logError . msg _CMP_SERVICE $ _ERROR_SERVICE_MIGRATION_METAMODEL__FAILED_CONVERT_TO_BSON collection
  logError . msg _CMP_SERVICE . show $ error

logMigrationFailedToMigrateCollection collection error = do
  logError . msg _CMP_SERVICE $ _ERROR_SERVICE_MIGRATION_METAMODEL__FAILED_TO_MIGRATE_COLLECTION collection
  logError . msg _CMP_SERVICE . show $ error
