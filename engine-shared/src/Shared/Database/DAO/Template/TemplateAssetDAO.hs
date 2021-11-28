module Shared.Database.DAO.Template.TemplateAssetDAO where

import Control.Lens ((^.))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, asks)
import Data.String
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import LensesConfig
import Shared.Database.DAO.Common
import Shared.Database.Mapping.Template.TemplateAsset ()
import Shared.Model.Context.ContextLenses
import Shared.Model.Error.Error
import Shared.Model.Template.Template

entityName = "template_asset"

findTemplateAssetsByTemplateId ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, HasAppUuid' s, MonadIO m)
  => String
  -> m [TemplateAsset]
findTemplateAssetsByTemplateId templateId = do
  appUuid <- asks (^. appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("template_id", templateId)]

findTemplateAssetsByTemplateIdAndFileName ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, HasAppUuid' s, MonadIO m)
  => String
  -> String
  -> m [TemplateAsset]
findTemplateAssetsByTemplateIdAndFileName templateId fileName = do
  appUuid <- asks (^. appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("template_id", templateId), ("file_name", fileName)]

findTemplateAssetById ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, HasAppUuid' s, MonadIO m)
  => String
  -> m TemplateAsset
findTemplateAssetById uuid = do
  appUuid <- asks (^. appUuid')
  createFindEntityByFn entityName [appQueryUuid appUuid, ("uuid", uuid)]

insertTemplateAsset ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, HasAppUuid' s, MonadIO m)
  => TemplateAsset
  -> m Int64
insertTemplateAsset = createInsertFn entityName

updateTemplateAssetById ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, HasAppUuid' s, MonadIO m)
  => TemplateAsset
  -> m Int64
updateTemplateAssetById asset = do
  appUuid <- asks (^. appUuid')
  let sql =
        fromString
          "UPDATE template_asset SET uuid = ?, asset_name = ?, content = ?, app_uuid = ? WHERE app_uuid = ? AND uuid = ?"
  let params = toRow asset ++ [toField appUuid, toField $ asset ^. uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTemplateAssets ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, HasAppUuid' s, MonadIO m) => m Int64
deleteTemplateAssets = createDeleteEntitiesFn entityName

deleteTemplateAssetsByTemplateId ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, HasAppUuid' s, MonadIO m)
  => String
  -> m Int64
deleteTemplateAssetsByTemplateId tmlId = do
  appUuid <- asks (^. appUuid')
  createDeleteEntitiesByFn entityName [appQueryUuid appUuid, ("template_id", tmlId)]

deleteTemplateAssetById ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, HasAppUuid' s, MonadIO m)
  => String
  -> m Int64
deleteTemplateAssetById uuid = do
  appUuid <- asks (^. appUuid')
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("uuid", uuid)]
