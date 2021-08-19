module Shared.Database.DAO.Template.TemplateFileDAO where

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
import Shared.Database.Mapping.Template.TemplateFile ()
import Shared.Model.Context.ContextLenses
import Shared.Model.Error.Error
import Shared.Model.Template.Template
import Shared.Util.Logger

entityName = "template_file"

findTemplateFiles ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => m [TemplateFile]
findTemplateFiles = createFindEntitiesFn entityName

findTemplateFilesByTemplateId ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => String -> m [TemplateFile]
findTemplateFilesByTemplateId templateId = createFindEntitiesByFn entityName [("template_id", templateId)]

findTemplateFilesByTemplateIdAndFileName ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m)
  => String
  -> String
  -> m [TemplateFile]
findTemplateFilesByTemplateIdAndFileName templateId fileName =
  createFindEntitiesByFn entityName [("template_id", templateId), ("file_name", fileName)]

findTemplateFileById ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => String -> m TemplateFile
findTemplateFileById = createFindEntityByFn entityName "uuid"

insertTemplateFile ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => TemplateFile -> m Int64
insertTemplateFile = createInsertFn entityName

updateTemplateFileById ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => TemplateFile -> m Int64
updateTemplateFileById file = do
  let params = toRow file ++ [toField $ file ^. uuid]
  let sql = "UPDATE template_file SET template_id = ?, uuid = ?, file_name = ?, content = ? WHERE uuid = ?"
  logInfo _CMP_DATABASE sql
  let action conn = execute conn (fromString sql) params
  runDB action

deleteTemplateFiles :: (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => m Int64
deleteTemplateFiles = createDeleteEntitiesFn entityName

deleteTemplateFilesByTemplateId ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => String -> m Int64
deleteTemplateFilesByTemplateId tmlId = createDeleteEntitiesByFn entityName [("template_id", tmlId)]

deleteTemplateFileById ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => String -> m Int64
deleteTemplateFileById = createDeleteEntityByFn entityName "uuid"
