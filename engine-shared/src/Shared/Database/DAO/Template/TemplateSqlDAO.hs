module Shared.Database.DAO.Template.TemplateSqlDAO where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import GHC.Int

import Shared.Database.BSON.Template.Template ()
import Shared.Database.BSON.Template.TemplateGroup ()
import Shared.Database.DAO.CommonSql
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

deleteTemplates :: (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => m Int64
deleteTemplates = createDeleteEntitiesFn entityName

deleteTemplatesFiltered ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => [(String, String)] -> m Int64
deleteTemplatesFiltered = createDeleteEntitiesByFn entityName

deleteTemplateById ::
     (MonadLogger m, MonadError AppError m, MonadReader s m, HasDbPool' s, MonadIO m) => String -> m Int64
deleteTemplateById = createDeleteEntityByFn entityName "id"
