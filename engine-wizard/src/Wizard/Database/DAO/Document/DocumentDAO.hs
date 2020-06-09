module Wizard.Database.DAO.Document.DocumentDAO where

import Data.Bson hiding (Document)
import qualified Data.ByteString as BS

import Shared.Database.DAO.Common
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Wizard.Database.BSON.Document.Document ()
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextLenses ()
import Wizard.Model.Document.Document

entityName = "document"

collection = "documents"

documentBucketName = "documentFs"

findDocuments :: AppContextM [Document]
findDocuments = createFindEntitiesFn collection

findDocumentsFiltered :: [(String, String)] -> AppContextM [Document]
findDocumentsFiltered queryParams = createFindEntitiesByFn collection (mapToDBQueryParams queryParams)

findDocumentsForCurrentUserPage :: Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page Document)
findDocumentsForCurrentUserPage mQtnUuid mQuery pageable sort =
  createFindEntitiesPageableQuerySortFn collection pageable sort =<<
  sel [regexSel "name" mQuery, textMaybeSel "questionnaireUuid" mQtnUuid, ownerUuidSel]

findDocumentById :: String -> AppContextM Document
findDocumentById = createFindEntityByFn collection entityName "uuid"

insertDocument :: Document -> AppContextM Value
insertDocument = createInsertFn collection

deleteDocuments :: AppContextM ()
deleteDocuments = createDeleteEntitiesFn collection

deleteDocumentsFiltered :: [(String, String)] -> AppContextM ()
deleteDocumentsFiltered queryParams = createDeleteEntitiesByFn collection (mapToDBQueryParams queryParams)

deleteDocumentById :: String -> AppContextM ()
deleteDocumentById = createDeleteEntityByFn collection "uuid"

findDocumentContent :: String -> AppContextM BS.ByteString
findDocumentContent = createFindFileFn documentBucketName

insertDocumentContent :: String -> BS.ByteString -> AppContextM ()
insertDocumentContent = createCreateFileFn documentBucketName

deleteDocumentContents :: AppContextM ()
deleteDocumentContents = createDeleteFilesFn documentBucketName

deleteDocumentContentsFiltered :: [(String, String)] -> AppContextM ()
deleteDocumentContentsFiltered queryParams = createDeleteFilesByFn documentBucketName (mapToDBQueryParams queryParams)

deleteDocumentContentById :: String -> AppContextM ()
deleteDocumentContentById = createDeleteFileByFn documentBucketName
