module Shared.Database.DAO.Template.TemplateDAO where

import Control.Lens ((^.))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import Data.Foldable (traverse_)
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

findTemplates :: (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => m [Template]
findTemplates = createFindEntitiesFn entityName

findTemplatesFiltered ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m)
  => [(String, String)]
  -> m [Template]
findTemplatesFiltered = createFindEntitiesByFn entityName

findTemplatesByOrganizationIdAndKmId ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m)
  => String
  -> String
  -> m [Template]
findTemplatesByOrganizationIdAndKmId organizationId templateId =
  createFindEntitiesByFn entityName [("organization_id", organizationId), ("template_id", templateId)]

findTemplateById ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => String -> m Template
findTemplateById = createFindEntityByFn entityName "id"

findTemplateById' ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => String -> m (Maybe Template)
findTemplateById' = createFindEntityByFn' entityName "id"

insertTemplate ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => Template -> m Int64
insertTemplate = createInsertFn entityName

updateTemplateById ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => Template -> m Int64
updateTemplateById template = do
  let params = toRow template ++ [toField $ template ^. tId]
  let action conn =
        execute
          conn
          " UPDATE template SET id = ?, name = ?, organization_id = ?, template_id = ?, version = ?, metamodel_version = ?, description = ?, readme = ?, license = ?, allowed_packages = ?, recommended_package_id = ?, formats = ?, created_at = ? WHERE id = ?"
          params
  runDB action

deleteTemplates :: (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => m Int64
deleteTemplates = do
  deleteTemplateFiles
  deleteTemplateAssets
  createDeleteEntitiesFn entityName

deleteTemplatesFiltered ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => [(String, String)] -> m Int64
deleteTemplatesFiltered params = do
  templates <- findTemplatesFiltered params
  traverse_ (\t -> deleteTemplateFilesByTemplateId (t ^. tId)) templates
  traverse_ (\t -> deleteTemplateAssetsByTemplateId (t ^. tId)) templates
  createDeleteEntitiesByFn entityName params

deleteTemplateById ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => String -> m Int64
deleteTemplateById templateId = do
  deleteTemplateFilesByTemplateId templateId
  deleteTemplateAssetsByTemplateId templateId
  createDeleteEntityByFn entityName "id" templateId
