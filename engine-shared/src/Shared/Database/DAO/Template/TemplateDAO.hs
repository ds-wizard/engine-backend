module Shared.Database.DAO.Template.TemplateDAO where

import Control.Lens ((^.))
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Bson hiding (Template)
import qualified Data.ByteString as BS

import LensesConfig
import Shared.Database.BSON.Template.Template ()
import Shared.Database.DAO.Common
import Shared.Model.Context.ContextLenses
import Shared.Model.Error.Error
import Shared.Model.Template.Template

entityName = "template"

collection = "templates"

templateAssetBucketName = "templateAssetFs"

findTemplates :: (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => m [Template]
findTemplates = createFindEntitiesFn collection

findTemplatesFiltered ::
     (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => [(String, String)] -> m [Template]
findTemplatesFiltered queryParams = createFindEntitiesByFn collection (mapToDBQueryParams queryParams)

findTemplatesByOrganizationIdAndKmId ::
     (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => String -> String -> m [Template]
findTemplatesByOrganizationIdAndKmId organizationId templateId =
  createFindEntitiesByFn collection ["organizationId" =: organizationId, "templateId" =: templateId]

findTemplateById :: (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => String -> m Template
findTemplateById = createFindEntityByFn collection entityName "id"

findTemplateById' :: (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => String -> m (Maybe Template)
findTemplateById' = createFindEntityByFn' collection entityName "id"

insertTemplate :: (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => Template -> m Value
insertTemplate = createInsertFn collection

updateTemplateById :: (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => Template -> m ()
updateTemplateById tml = createUpdateByFn collection "id" (tml ^. tId) tml

deleteTemplates :: (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => m ()
deleteTemplates = createDeleteEntitiesFn collection

deleteTemplatesFiltered :: (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => [(String, String)] -> m ()
deleteTemplatesFiltered queryParams = createDeleteEntitiesByFn collection (mapToDBQueryParams queryParams)

deleteTemplateById :: (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => String -> m ()
deleteTemplateById = createDeleteEntityByFn collection "id"

findTemplateAssetContent :: (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => String -> m BS.ByteString
findTemplateAssetContent = createFindFileFn templateAssetBucketName

insertTemplateAssetContent ::
     (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => String -> BS.ByteString -> m ()
insertTemplateAssetContent = createCreateFileFn templateAssetBucketName

deleteTemplateAssetContents :: (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => m ()
deleteTemplateAssetContents = createDeleteFilesFn templateAssetBucketName

deleteTemplateAssetContentsFiltered ::
     (MonadError AppError m, MonadReader s m, HasPool' s, MonadIO m) => [(String, String)] -> m ()
deleteTemplateAssetContentsFiltered queryParams =
  createDeleteFilesByFn templateAssetBucketName (mapToDBQueryParams queryParams)
