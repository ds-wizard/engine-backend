module WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import WizardLib.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplateFile ()
import WizardLib.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplateFileList ()
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFileList

entityName = "document_template_file"

findFilesByDocumentTemplateId :: AppContextC s sc m => String -> m [DocumentTemplateFile]
findFilesByDocumentTemplateId documentTemplateId = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("document_template_id", documentTemplateId)]

findFileListsByDocumentTemplateId :: AppContextC s sc m => String -> m [DocumentTemplateFileList]
findFileListsByDocumentTemplateId documentTemplateId = do
  appUuid <- asks (.appUuid')
  createFindEntitiesWithFieldsByFn "uuid, file_name, created_at, updated_at" entityName [appQueryUuid appUuid, ("document_template_id", documentTemplateId)]

findFilesByDocumentTemplateIdAndFileName :: AppContextC s sc m => String -> String -> m [DocumentTemplateFile]
findFilesByDocumentTemplateIdAndFileName documentTemplateId fileName = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("document_template_id", documentTemplateId), ("file_name", fileName)]

findFileById :: AppContextC s sc m => U.UUID -> m DocumentTemplateFile
findFileById uuid = do
  appUuid <- asks (.appUuid')
  createFindEntityByFn entityName [appQueryUuid appUuid, ("uuid", U.toString uuid)]

insertFile :: AppContextC s sc m => DocumentTemplateFile -> m Int64
insertFile = createInsertFn entityName

updateFileById :: AppContextC s sc m => DocumentTemplateFile -> m Int64
updateFileById file = do
  appUuid <- asks (.appUuid')
  let sql =
        fromString
          "UPDATE document_template_file SET document_template_id = ?, uuid = ?, file_name = ?, content = ?, app_uuid = ?, created_at = ?, updated_at = ? WHERE app_uuid = ? AND uuid = ?"
  let params = toRow file ++ [toField appUuid, toField file.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteFiles :: AppContextC s sc m => m Int64
deleteFiles = createDeleteEntitiesFn entityName

deleteFilesByDocumentTemplateId :: AppContextC s sc m => String -> m Int64
deleteFilesByDocumentTemplateId tmlId = do
  appUuid <- asks (.appUuid')
  createDeleteEntitiesByFn entityName [appQueryUuid appUuid, ("document_template_id", tmlId)]

deleteFileById :: AppContextC s sc m => U.UUID -> m Int64
deleteFileById uuid = do
  appUuid <- asks (.appUuid')
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("uuid", U.toString uuid)]
