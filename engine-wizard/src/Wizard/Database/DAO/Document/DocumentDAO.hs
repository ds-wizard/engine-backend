module Wizard.Database.DAO.Document.DocumentDAO where

import Data.Maybe
import Data.String
import Database.PostgreSQL.Simple
import GHC.Int

import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Document.Document ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Document.Document
import Wizard.Util.Logger

entityName = "document"

pageLabel = "documents"

findDocuments :: AppContextM [Document]
findDocuments = createFindEntitiesFn entityName

findDocumentsFiltered :: [(String, String)] -> AppContextM [Document]
findDocumentsFiltered = createFindEntitiesByFn entityName

findDocumentsPage :: Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page Document)
findDocumentsPage mQtnUuid mQuery pageable sort =
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "*"
    (if isJust mQtnUuid
       then "name ~* ? AND questionnaire_uuid = ? AND durability='PersistentDocumentDurability'"
       else "name ~* ? AND durability='PersistentDocumentDurability'")
    (regex mQuery : maybeToList mQtnUuid)

findDocumentsByQuestionnaireUuidPage :: String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page Document)
findDocumentsByQuestionnaireUuidPage qtnUuid mQuery pageable sort =
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "*"
    "name ~* ? AND questionnaire_uuid = ? AND durability='PersistentDocumentDurability'"
    (regex mQuery : [qtnUuid])

findDocumentsByTemplateId :: String -> AppContextM [Document]
findDocumentsByTemplateId templateId = createFindEntitiesByFn entityName [("template_id", templateId)]

findDocumentById :: String -> AppContextM Document
findDocumentById = createFindEntityByFn entityName "uuid"

insertDocument :: Document -> AppContextM Int64
insertDocument = createInsertFn entityName

deleteDocuments :: AppContextM Int64
deleteDocuments = createDeleteEntitiesFn entityName

deleteDocumentsFiltered :: [(String, String)] -> AppContextM Int64
deleteDocumentsFiltered = createDeleteEntitiesByFn entityName

deleteDocumentById :: String -> AppContextM Int64
deleteDocumentById = createDeleteEntityByFn entityName "uuid"

deleteTemporalDocumentsByTemplateId :: String -> AppContextM Int64
deleteTemporalDocumentsByTemplateId templateId =
  deleteDocumentsFiltered [("template_id", templateId), ("durability", "TemporallyDocumentDurability")]

deleteTemporalDocumentsByTemplateAssetId :: String -> AppContextM Int64
deleteTemporalDocumentsByTemplateAssetId = deleteTemporalDocumentsByTableAndId "template_asset"

deleteTemporalDocumentsByTemplateFileId :: String -> AppContextM Int64
deleteTemporalDocumentsByTemplateFileId = deleteTemporalDocumentsByTableAndId "template_file"

-- --------------------------------
-- PRIVATE
-- --------------------------------
deleteTemporalDocumentsByTableAndId :: String -> String -> AppContextM Int64
deleteTemporalDocumentsByTableAndId joinTableName entityUuid = do
  let sql =
        f'
          "DELETE \
          \FROM document \
          \WHERE uuid IN ( \
          \    SELECT d.uuid \
          \    FROM %s join_table \
          \             JOIN document d ON join_table.template_id = d.template_id \
          \    WHERE join_table.uuid = '%s' \
          \      AND d.durability = 'TemporallyDocumentDurability' \
          \)"
          [joinTableName, entityUuid]
  logInfoU _CMP_DATABASE sql
  let action conn = execute_ conn (fromString sql)
  runDB action
