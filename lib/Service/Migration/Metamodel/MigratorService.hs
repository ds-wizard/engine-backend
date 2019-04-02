module Service.Migration.Metamodel.MigratorService
  ( migrateKnowledgeModelBundle
  , migrateCompleteDatabase
  -- Helpers
  , heMigrateKnowledgeModelBundle
  ) where

import Data.Aeson
import Data.Bson.Generic
import qualified Data.Text as T
import Database.MongoDB
       ((=:), delete, find, findOne, insert, insertMany, rest, select)

import Api.Resource.Branch.BranchWithEventsJM ()
import Api.Resource.Migration.MigratorStateDetailJM ()
import Constant.Component
import Constant.KnowledgeModel
import Database.BSON.Branch.BranchWithEvents ()
import Database.BSON.Migrator.MigratorState ()
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
       Service.Migration.Metamodel.Migrator.KnowledgeModelBundleMigrator
       as KMBMigrator
import qualified
       Service.Migration.Metamodel.Migrator.KnowledgeModelMigrationMigrator
       as KMMMigrator
import qualified
       Service.Migration.Metamodel.Migrator.PackageMigrator
       as PackageMigrator
import qualified Service.Package.PackageMapper as PackageMapper
import Util.BSONtoJSON (mapBSONDocumentToJSONObject)
import Util.List (foldEither)
import Util.Logger (logError, msg)

migrateKnowledgeModelBundle :: Value -> AppContextM (Either AppError Value)
migrateKnowledgeModelBundle = return . KMBMigrator.migrate

migrateCompleteDatabase :: AppContextM ()
migrateCompleteDatabase = do
  migratePackages
  migrateBranches
  migrateKnowledgeModelMigrations
  migratePublicQuestionnaire

-- ---------------------------
-- PRIVATE
-- ---------------------------
migratePackages :: AppContextM ()
migratePackages = migrateOutdatedModels "packages" PackageMapper.fromDTOWithEvents PackageMigrator.migrate

migrateBranches :: AppContextM ()
migrateBranches = migrateOutdatedModels "branches" BranchMapper.fromWithEventsDTO BranchMigrator.migrate

migrateKnowledgeModelMigrations :: AppContextM ()
migrateKnowledgeModelMigrations =
  migrateOutdatedModels "kmMigrations" KMMigratorMapper.fromDetailDTO KMMMigrator.migrate

migratePublicQuestionnaire :: AppContextM ()
migratePublicQuestionnaire = do
  mPackage <- runDB $ findOne (select [] "publicPackages")
  case mPackage of
    Just package ->
      case PackageMigrator.migrate . Object . mapBSONDocumentToJSONObject $ package of
        Right upgradedEntity -> do
          runDB . delete $ select [] "publicPackages"
          case eitherDecode . encode $ upgradedEntity of
            Right object -> do
              let document = toBSON . PackageMapper.fromDTOWithEvents $ object
              runDB $ insert "publicPackages" document
              return ()
            Left error -> do
              logError . msg _CMP_SERVICE $ _ERROR_SERVICE_MIGRATION_METAMODEL__FAILED_CONVERT_TO_BSON "publicPackages"
              return ()
        Left error -> do
          logError . msg _CMP_SERVICE $
            _ERROR_SERVICE_MIGRATION_METAMODEL__FAILED_TO_MIGRATE_COLLECTION "PublicQuestionnaire"
          return ()
    Nothing -> return ()

findOutdatedModels collection =
  runDB $ rest =<< find (select ["metamodelVersion" =: ["$ne" =: kmMetamodelVersion]] (T.pack collection))

removeOutdatedModels collection =
  runDB . delete $ select ["metamodelVersion" =: ["$ne" =: kmMetamodelVersion]] (T.pack collection)

insertUpdatedModels collection models = runDB $ insertMany (T.pack collection) models

migrateOutdatedModels collection dtoMapper migrateFn = do
  entities <- findOutdatedModels collection
  case foldEither $ migrateFn . Object . mapBSONDocumentToJSONObject <$> entities of
    Right upgradedEntities -> do
      removeOutdatedModels collection
      case eitherDecode . encode $ upgradedEntities of
        Right objects -> do
          let documents = toBSON <$> (dtoMapper <$> objects)
          insertUpdatedModels collection documents
          return ()
        Left error -> do
          logError . msg _CMP_SERVICE $ _ERROR_SERVICE_MIGRATION_METAMODEL__FAILED_CONVERT_TO_BSON collection
          return ()
    Left error -> do
      logError $ msg _CMP_SERVICE $ _ERROR_SERVICE_MIGRATION_METAMODEL__FAILED_TO_MIGRATE_COLLECTION collection
      return ()

-- ---------------------------
-- HELPERS
-- ---------------------------
heMigrateKnowledgeModelBundle encodedKmb callback = do
  eitherResult <- migrateKnowledgeModelBundle encodedKmb
  case eitherResult of
    Right result -> callback result
    Left error -> return . Left $ error
