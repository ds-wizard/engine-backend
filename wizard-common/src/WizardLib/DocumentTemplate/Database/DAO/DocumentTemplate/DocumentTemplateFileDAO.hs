module WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO where

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

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Error.Error
import WizardLib.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplateFile ()
import WizardLib.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplateFileList ()
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFileList

entityName = "document_template_file"

findFilesByDocumentTemplateId
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
  -> m [DocumentTemplateFile]
findFilesByDocumentTemplateId documentTemplateId = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("document_template_id", documentTemplateId)]

findFileListsByDocumentTemplateId
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
  -> m [DocumentTemplateFileList]
findFileListsByDocumentTemplateId documentTemplateId = do
  appUuid <- asks (.appUuid')
  createFindEntitiesWithFieldsByFn "uuid, file_name, created_at, updated_at" entityName [appQueryUuid appUuid, ("document_template_id", documentTemplateId)]

findFilesByDocumentTemplateIdAndFileName
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
  -> m [DocumentTemplateFile]
findFilesByDocumentTemplateIdAndFileName documentTemplateId fileName = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("document_template_id", documentTemplateId), ("file_name", fileName)]

findFileById
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
  -> m DocumentTemplateFile
findFileById uuid = do
  appUuid <- asks (.appUuid')
  createFindEntityByFn entityName [appQueryUuid appUuid, ("uuid", U.toString uuid)]

insertFile
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
  => DocumentTemplateFile
  -> m Int64
insertFile = createInsertFn entityName

updateFileById
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
  => DocumentTemplateFile
  -> m Int64
updateFileById file = do
  appUuid <- asks (.appUuid')
  let sql =
        fromString
          "UPDATE document_template_file SET document_template_id = ?, uuid = ?, file_name = ?, content = ?, app_uuid = ?, created_at = ?, updated_at = ? WHERE app_uuid = ? AND uuid = ?"
  let params = toRow file ++ [toField appUuid, toField file.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteFiles
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
deleteFiles = createDeleteEntitiesFn entityName

deleteFilesByDocumentTemplateId
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
deleteFilesByDocumentTemplateId tmlId = do
  appUuid <- asks (.appUuid')
  createDeleteEntitiesByFn entityName [appQueryUuid appUuid, ("document_template_id", tmlId)]

deleteFileById
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
deleteFileById uuid = do
  appUuid <- asks (.appUuid')
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("uuid", U.toString uuid)]
