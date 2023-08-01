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
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("document_template_id", documentTemplateId)]

findAssetsByDocumentTemplateIdAndFileName :: AppContextC s sc m => String -> String -> m [DocumentTemplateAsset]
findAssetsByDocumentTemplateIdAndFileName documentTemplateId fileName = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("document_template_id", documentTemplateId), ("file_name", fileName)]

findAssetById :: AppContextC s sc m => U.UUID -> m DocumentTemplateAsset
findAssetById uuid = do
  appUuid <- asks (.appUuid')
  createFindEntityByFn entityName [appQueryUuid appUuid, ("uuid", U.toString uuid)]

sumAssetFileSize :: AppContextC s sc m => m Int64
sumAssetFileSize = do
  appUuid <- asks (.appUuid')
  sumAssetFileSizeWithApp appUuid

sumAssetFileSizeWithApp :: AppContextC s sc m => U.UUID -> m Int64
sumAssetFileSizeWithApp appUuid = createSumByFn entityName "file_size" appCondition [U.toString appUuid]

insertAsset :: AppContextC s sc m => DocumentTemplateAsset -> m Int64
insertAsset = createInsertFn entityName

updateAssetById :: AppContextC s sc m => DocumentTemplateAsset -> m Int64
updateAssetById asset = do
  appUuid <- asks (.appUuid')
  let sql =
        fromString
          "UPDATE document_template_asset SET document_template_id = ?, uuid = ?, file_name = ?, content_type = ?, app_uuid = ?, file_size = ?, created_at = ?, updated_at = ? WHERE app_uuid = ? AND uuid = ?"
  let params = toRow asset ++ [toField appUuid, toField asset.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteAssets :: AppContextC s sc m => m Int64
deleteAssets = createDeleteEntitiesFn entityName

deleteAssetsByDocumentTemplateId :: AppContextC s sc m => String -> m Int64
deleteAssetsByDocumentTemplateId tmlId = do
  appUuid <- asks (.appUuid')
  createDeleteEntitiesByFn entityName [appQueryUuid appUuid, ("document_template_id", tmlId)]

deleteAssetById :: AppContextC s sc m => U.UUID -> m Int64
deleteAssetById uuid = do
  appUuid <- asks (.appUuid')
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("uuid", U.toString uuid)]
