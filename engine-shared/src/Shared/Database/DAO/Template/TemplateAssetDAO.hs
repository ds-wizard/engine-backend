module Shared.Database.DAO.Template.TemplateAssetDAO where

import Control.Lens ((^.))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
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
import Shared.Util.Logger

entityName = "template_asset"

findTemplateAssets ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => m [TemplateAsset]
findTemplateAssets = createFindEntitiesFn entityName

findTemplateAssetsByTemplateId ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => String -> m [TemplateAsset]
findTemplateAssetsByTemplateId templateId = createFindEntitiesByFn entityName [("template_id", templateId)]

findTemplateAssetById ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => String -> m TemplateAsset
findTemplateAssetById = createFindEntityByFn entityName "uuid"

insertTemplateAsset ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => TemplateAsset -> m Int64
insertTemplateAsset = createInsertFn entityName

updateTemplateAssetById ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => TemplateAsset -> m Int64
updateTemplateAssetById asset = do
  let params = toRow asset ++ [toField $ asset ^. uuid]
  let sql = "UPDATE template_asset SET uuid = ?, asset_name = ?, content = ? WHERE uuid = ?"
  logInfo _CMP_DATABASE sql
  let action conn = execute conn (fromString sql) params
  runDB action

deleteTemplateAssets :: (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => m Int64
deleteTemplateAssets = createDeleteEntitiesFn entityName

deleteTemplateAssetsByTemplateId ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => String -> m Int64
deleteTemplateAssetsByTemplateId tmlId = createDeleteEntitiesByFn entityName [("template_id", tmlId)]

deleteTemplateAssetById ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => String -> m Int64
deleteTemplateAssetById = createDeleteEntityByFn entityName "uuid"
