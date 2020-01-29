module Wizard.Database.DAO.Document.DocumentDAO where

import Data.Bson hiding (Document)
import qualified Data.ByteString as BS
import Data.Text (Text)

import Shared.Model.Error.Error
import Shared.Util.Helper (createHeeHelper, createHemHelper)
import Wizard.Database.BSON.Document.Document ()
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document

entityName = "document"

collection = "documents"

documentBucketName = "documentFs"

findDocuments :: AppContextM (Either AppError [Document])
findDocuments = createFindEntitiesFn collection

findDocumentsFiltered :: [(Text, Text)] -> AppContextM (Either AppError [Document])
findDocumentsFiltered queryParams = createFindEntitiesByFn collection (mapToDBQueryParams queryParams)

findDocumentById :: String -> AppContextM (Either AppError Document)
findDocumentById = createFindEntityByFn collection entityName "uuid"

insertDocument :: Document -> AppContextM Value
insertDocument = createInsertFn collection

deleteDocuments :: AppContextM ()
deleteDocuments = createDeleteEntitiesFn collection

deleteDocumentById :: String -> AppContextM ()
deleteDocumentById = createDeleteEntityByFn collection "uuid"

findDocumentContent :: String -> AppContextM (Either AppError BS.ByteString)
findDocumentContent = createFindFileFn documentBucketName

insertDocumentContent :: String -> BS.ByteString -> AppContextM ()
insertDocumentContent = createCreateFileFn documentBucketName

deleteDocumentContents :: AppContextM ()
deleteDocumentContents = createDeleteFilesFn documentBucketName

deleteDocumentContentById :: String -> AppContextM ()
deleteDocumentContentById = createDeleteFileByFn documentBucketName

-- --------------------------------
-- HELPERS
-- --------------------------------
heFindDocuments = createHeeHelper findDocuments

-- --------------------------------
heFindDocumentsFiltered queryParams = createHeeHelper (findDocumentsFiltered queryParams)

-- --------------------------------
heFindDocumentById docUuid = createHeeHelper (findDocumentById docUuid)

hmFindDocumentById docUuid = createHemHelper (findDocumentById docUuid)

-- --------------------------------
heFindDocumentContent docUuid = createHeeHelper (findDocumentContent docUuid)
