module Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplateFile ()
import Shared.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplateFileList ()
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFileList

entityName = "document_template_file"

findFilesByDocumentTemplateId :: AppContextC s sc m => String -> m [DocumentTemplateFile]
findFilesByDocumentTemplateId documentTemplateId = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_id", documentTemplateId)]

findFileListsByDocumentTemplateId :: AppContextC s sc m => String -> m [DocumentTemplateFileList]
findFileListsByDocumentTemplateId documentTemplateId = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesWithFieldsByFn "uuid, file_name, created_at, updated_at" entityName [tenantQueryUuid tenantUuid, ("document_template_id", documentTemplateId)]

findFilesByDocumentTemplateIdAndFileName :: AppContextC s sc m => String -> String -> m [DocumentTemplateFile]
findFilesByDocumentTemplateIdAndFileName documentTemplateId fileName = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_id", documentTemplateId), ("file_name", fileName)]

findFileById :: AppContextC s sc m => U.UUID -> m DocumentTemplateFile
findFileById uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

insertFile :: AppContextC s sc m => DocumentTemplateFile -> m Int64
insertFile = createInsertFn entityName

updateFileById :: AppContextC s sc m => DocumentTemplateFile -> m Int64
updateFileById file = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "UPDATE document_template_file SET document_template_id = ?, uuid = ?, file_name = ?, content = ?, tenant_uuid = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = toRow file ++ [toField tenantUuid, toField file.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteFiles :: AppContextC s sc m => m Int64
deleteFiles = createDeleteEntitiesFn entityName

deleteFilesByDocumentTemplateId :: AppContextC s sc m => String -> m Int64
deleteFilesByDocumentTemplateId tmlId = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_id", tmlId)]

deleteFileById :: AppContextC s sc m => U.UUID -> m Int64
deleteFileById uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
