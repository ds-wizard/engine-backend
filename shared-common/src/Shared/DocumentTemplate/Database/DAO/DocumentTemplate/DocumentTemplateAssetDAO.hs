module Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplateAsset ()
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

entityName = "document_template_asset"

findAssetsByDocumentTemplateUuid :: AppContextC s sc m => U.UUID -> m [DocumentTemplateAsset]
findAssetsByDocumentTemplateUuid documentTemplateUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString documentTemplateUuid)]

findAssetsByDocumentTemplateUuidAndFileName :: AppContextC s sc m => U.UUID -> String -> m [DocumentTemplateAsset]
findAssetsByDocumentTemplateUuidAndFileName documentTemplateUuid fileName = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString documentTemplateUuid), ("file_name", fileName)]

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
          "UPDATE document_template_asset SET document_template_uuid = ?, uuid = ?, file_name = ?, content_type = ?, tenant_uuid = ?, file_size = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = toRow asset ++ [toField tenantUuid, toField asset.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteAssets :: AppContextC s sc m => m Int64
deleteAssets = createDeleteEntitiesFn entityName

deleteAssetsByDocumentTemplateUuid :: AppContextC s sc m => U.UUID -> m Int64
deleteAssetsByDocumentTemplateUuid documentTemplateUuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString documentTemplateUuid)]

deleteAssetById :: AppContextC s sc m => U.UUID -> m Int64
deleteAssetById uuid = do
  tenantUuid <- asks (.tenantUuid')
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
