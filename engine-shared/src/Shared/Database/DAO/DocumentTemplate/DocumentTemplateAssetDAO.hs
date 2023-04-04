module Shared.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, asks)
import Data.Pool
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int
import GHC.Records

import Shared.Database.DAO.Common
import Shared.Database.Mapping.DocumentTemplate.DocumentTemplateAsset ()
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Error.Error

entityName = "document_template_asset"

findAssetsByDocumentTemplateId
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => String
  -> m [DocumentTemplateAsset]
findAssetsByDocumentTemplateId documentTemplateId = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("document_template_id", documentTemplateId)]

findAssetsByDocumentTemplateIdAndFileName
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => String
  -> String
  -> m [DocumentTemplateAsset]
findAssetsByDocumentTemplateIdAndFileName documentTemplateId fileName = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("document_template_id", documentTemplateId), ("file_name", fileName)]

findAssetById
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => U.UUID
  -> m DocumentTemplateAsset
findAssetById uuid = do
  appUuid <- asks (.appUuid')
  createFindEntityByFn entityName [appQueryUuid appUuid, ("uuid", U.toString uuid)]

sumAssetFileSize
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => m Int64
sumAssetFileSize = do
  appUuid <- asks (.appUuid')
  sumAssetFileSizeWithApp appUuid

sumAssetFileSizeWithApp
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => U.UUID
  -> m Int64
sumAssetFileSizeWithApp appUuid = createSumByFn entityName "file_size" appCondition [U.toString appUuid]

insertAsset
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => DocumentTemplateAsset
  -> m Int64
insertAsset = createInsertFn entityName

updateAssetById
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => DocumentTemplateAsset
  -> m Int64
updateAssetById asset = do
  appUuid <- asks (.appUuid')
  let sql =
        fromString
          "UPDATE document_template_asset SET document_template_id = ?, uuid = ?, file_name = ?, content_type = ?, app_uuid = ?, file_size = ?, created_at = ?, updated_at = ? WHERE app_uuid = ? AND uuid = ?"
  let params = toRow asset ++ [toField appUuid, toField asset.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteAssets
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => m Int64
deleteAssets = createDeleteEntitiesFn entityName

deleteAssetsByDocumentTemplateId
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => String
  -> m Int64
deleteAssetsByDocumentTemplateId tmlId = do
  appUuid <- asks (.appUuid')
  createDeleteEntitiesByFn entityName [appQueryUuid appUuid, ("document_template_id", tmlId)]

deleteAssetById
  :: ( MonadLogger m
     , MonadError AppError m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => U.UUID
  -> m Int64
deleteAssetById uuid = do
  appUuid <- asks (.appUuid')
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("uuid", U.toString uuid)]
