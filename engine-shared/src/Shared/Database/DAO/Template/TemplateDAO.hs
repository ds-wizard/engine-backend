module Shared.Database.DAO.Template.TemplateDAO where

import Control.Lens ((^.))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader, asks)
import Data.Foldable (traverse_)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import LensesConfig
import Shared.Database.DAO.Common
import Shared.Database.DAO.Template.TemplateAssetDAO (deleteTemplateAssets, deleteTemplateAssetsByTemplateId)
import Shared.Database.DAO.Template.TemplateFileDAO (deleteTemplateFiles, deleteTemplateFilesByTemplateId)
import Shared.Database.Mapping.Template.Template ()
import Shared.Model.Context.ContextLenses
import Shared.Model.Error.Error
import Shared.Model.Template.Template

entityName = "template"

findTemplates ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, HasAppUuid' s, MonadIO m) => m [Template]
findTemplates = do
  appUuid <- asks (^. appUuid')
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findTemplatesFiltered ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, HasAppUuid' s, MonadIO m)
  => [(String, String)]
  -> m [Template]
findTemplatesFiltered queryParams = do
  appUuid <- asks (^. appUuid')
  createFindEntitiesByFn entityName (appQueryUuid appUuid : queryParams)

findTemplatesByOrganizationIdAndKmId ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, HasAppUuid' s, MonadIO m)
  => String
  -> String
  -> m [Template]
findTemplatesByOrganizationIdAndKmId organizationId templateId = do
  appUuid <- asks (^. appUuid')
  createFindEntitiesByFn
    entityName
    [appQueryUuid appUuid, ("organization_id", organizationId), ("template_id", templateId)]

findTemplateById ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, HasAppUuid' s, MonadIO m)
  => String
  -> m Template
findTemplateById id = do
  appUuid <- asks (^. appUuid')
  createFindEntityByFn entityName [appQueryUuid appUuid, ("id", id)]

findTemplateById' ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, HasAppUuid' s, MonadIO m)
  => String
  -> m (Maybe Template)
findTemplateById' id = do
  appUuid <- asks (^. appUuid')
  createFindEntityByFn' entityName [appQueryUuid appUuid, ("id", id)]

countTemplatesGroupedByOrganizationIdAndKmId ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, HasAppUuid' s, MonadIO m) => m Int
countTemplatesGroupedByOrganizationIdAndKmId = do
  appUuid <- asks (^. appUuid')
  let sql =
        "SELECT COUNT(*) \
            \FROM (SELECT 1 \
            \      FROM template \
            \      WHERE app_uuid = ? \
            \      GROUP BY organization_id, template_id) nested;"
  let params = [U.toString appUuid]
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

insertTemplate ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, HasAppUuid' s, MonadIO m)
  => Template
  -> m Int64
insertTemplate = createInsertFn entityName

updateTemplateById ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, HasAppUuid' s, MonadIO m)
  => Template
  -> m Int64
updateTemplateById template = do
  appUuid <- asks (^. appUuid')
  let sql =
        fromString
          "UPDATE template SET id = ?, name = ?, organization_id = ?, template_id = ?, version = ?, metamodel_version = ?, description = ?, readme = ?, license = ?, allowed_packages = ?, recommended_package_id = ?, formats = ?, created_at = ?, app_uuid = ? WHERE app_uuid = ? AND id = ?"
  let params = toRow template ++ [toField appUuid, toField $ template ^. tId]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteTemplates ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, HasAppUuid' s, MonadIO m) => m Int64
deleteTemplates = do
  deleteTemplateFiles
  deleteTemplateAssets
  createDeleteEntitiesFn entityName

deleteTemplatesFiltered ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, HasAppUuid' s, MonadIO m)
  => [(String, String)]
  -> m Int64
deleteTemplatesFiltered params = do
  appUuid <- asks (^. appUuid')
  templates <- findTemplatesFiltered params
  traverse_ (\t -> deleteTemplateFilesByTemplateId (t ^. tId)) templates
  traverse_ (\t -> deleteTemplateAssetsByTemplateId (t ^. tId)) templates
  createDeleteEntitiesByFn entityName (appQueryUuid appUuid : params)

deleteTemplateById ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, HasAppUuid' s, MonadIO m)
  => String
  -> m Int64
deleteTemplateById templateId = do
  appUuid <- asks (^. appUuid')
  deleteTemplateFilesByTemplateId templateId
  deleteTemplateAssetsByTemplateId templateId
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("id", templateId)]
