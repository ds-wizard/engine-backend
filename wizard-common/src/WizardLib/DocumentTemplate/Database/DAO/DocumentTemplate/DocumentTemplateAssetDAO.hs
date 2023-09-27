module WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import WizardLib.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplateAsset ()
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

entityName = "document_template_asset"

findAssetsByDocumentTemplateId :: AppContextC s sc m => String -> m [DocumentTemplateAsset]
findAssetsByDocumentTemplateId documentTemplateId = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_id", documentTemplateId)]

findAssetsByDocumentTemplateIdAndFileName :: AppContextC s sc m => String -> String -> m [DocumentTemplateAsset]
findAssetsByDocumentTemplateIdAndFileName documentTemplateId fileName = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_id", documentTemplateId), ("file_name", fileName)]

findAssetById :: AppContextC s sc m => U.UUID -> m DocumentTemplateAsset
findAssetById uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

sumAssetFileSize :: AppContextC s sc m => m Int64
sumAssetFileSize = do
  tenantUuid <- asks (.tenantUuid')
  sumAssetFileSizeWithTenant tenantUuid

sumAssetFileSizeWithTenant :: AppContextC s sc m => U.UUID -> m Int64
sumAssetFileSizeWithTenant tenantUuid = createSumByFn entityName "file_size" tenantCondition [U.toString tenantUuid]

insertAsset :: AppContextC s sc m => DocumentTemplateAsset -> m Int64
insertAsset = createInsertFn entityName

updateAssetById :: AppContextC s sc m => DocumentTemplateAsset -> m Int64
updateAssetById asset = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "UPDATE document_template_asset SET document_template_id = ?, uuid = ?, file_name = ?, content_type = ?, tenant_uuid = ?, file_size = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = toRow asset ++ [toField tenantUuid, toField asset.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteAssets :: AppContextC s sc m => m Int64
deleteAssets = createDeleteEntitiesFn entityName

deleteAssetsByDocumentTemplateId :: AppContextC s sc m => String -> m Int64
deleteAssetsByDocumentTemplateId tmlId = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_id", tmlId)]

deleteAssetById :: AppContextC s sc m => U.UUID -> m Int64
deleteAssetById uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
