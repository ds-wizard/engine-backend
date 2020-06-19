module Registry.Database.DAO.Template.TemplateDAO where

import Control.Lens ((^.))
import Data.Bson hiding (Template)
import qualified Data.ByteString as BS

import LensesConfig
import Registry.Model.Context.AppContext
import Registry.Model.Context.AppContextLenses ()
import Shared.Database.BSON.Template.Template ()
import Shared.Database.DAO.Common
import Shared.Model.Template.Template

entityName = "template"

collection = "templates"

templateAssetBucketName = "templateAssetFs"

findTemplates :: AppContextM [Template]
findTemplates = createFindEntitiesFn collection

findTemplateById :: String -> AppContextM Template
findTemplateById = createFindEntityByFn collection entityName "id"

insertTemplate :: Template -> AppContextM Value
insertTemplate = createInsertFn collection

updateTemplateById :: Template -> AppContextM ()
updateTemplateById tml = createUpdateByFn collection "id" (tml ^. tId) tml

deleteTemplates :: AppContextM ()
deleteTemplates = createDeleteEntitiesFn collection

deleteTemplateById :: String -> AppContextM ()
deleteTemplateById = createDeleteEntityByFn collection "id"

findTemplateAssetContent :: String -> AppContextM BS.ByteString
findTemplateAssetContent = createFindFileFn templateAssetBucketName

insertTemplateAssetContent :: String -> BS.ByteString -> AppContextM ()
insertTemplateAssetContent = createCreateFileFn templateAssetBucketName

deleteTemplateAssetContents :: AppContextM ()
deleteTemplateAssetContents = createDeleteFilesFn templateAssetBucketName

deleteTemplateAssetContentsFiltered :: [(String, String)] -> AppContextM ()
deleteTemplateAssetContentsFiltered queryParams =
  createDeleteFilesByFn templateAssetBucketName (mapToDBQueryParams queryParams)
