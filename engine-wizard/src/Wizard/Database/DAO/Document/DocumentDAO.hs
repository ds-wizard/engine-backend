module Wizard.Database.DAO.Document.DocumentDAO where

import Data.Bson hiding (Document)
import qualified Data.ByteString as BS

import Wizard.Database.BSON.Document.Document ()
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document

entityName = "document"

collection = "documents"

documentBucketName = "documentFs"

findDocuments :: AppContextM [Document]
findDocuments = createFindEntitiesFn collection

findDocumentsFiltered :: [(String, String)] -> AppContextM [Document]
findDocumentsFiltered queryParams = createFindEntitiesByFn collection (mapToDBQueryParams queryParams)

findDocumentById :: String -> AppContextM Document
findDocumentById = createFindEntityByFn collection entityName "uuid"

insertDocument :: Document -> AppContextM Value
insertDocument = createInsertFn collection

deleteDocuments :: AppContextM ()
deleteDocuments = createDeleteEntitiesFn collection

deleteDocumentById :: String -> AppContextM ()
deleteDocumentById = createDeleteEntityByFn collection "uuid"

findDocumentContent :: String -> AppContextM BS.ByteString
findDocumentContent = createFindFileFn documentBucketName

insertDocumentContent :: String -> BS.ByteString -> AppContextM ()
insertDocumentContent = createCreateFileFn documentBucketName

deleteDocumentContents :: AppContextM ()
deleteDocumentContents = createDeleteFilesFn documentBucketName

deleteDocumentContentById :: String -> AppContextM ()
deleteDocumentContentById = createDeleteFileByFn documentBucketName
