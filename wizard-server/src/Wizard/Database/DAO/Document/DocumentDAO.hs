module Wizard.Database.DAO.Document.DocumentDAO where

import Control.Monad.Reader (asks)
import Data.Maybe
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Document.Document ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Document.Document

entityName = "document"

pageLabel = "documents"

findDocuments :: AppContextM [Document]
findDocuments = do
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findDocumentsFiltered :: [(String, String)] -> AppContextM [Document]
findDocumentsFiltered params = do
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName (appQueryUuid appUuid : params)

findDocumentsPage :: Maybe U.UUID -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page Document)
findDocumentsPage mQtnUuid mQuery pageable sort = do
  appUuid <- asks currentAppUuid
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "*"
    ( if isJust mQtnUuid
        then "WHERE app_uuid = ? AND name ~* ? AND questionnaire_uuid = ? AND durability='PersistentDocumentDurability'"
        else "WHERE app_uuid = ? AND name ~* ? AND durability='PersistentDocumentDurability'"
    )
    (U.toString appUuid : regex mQuery : maybeToList (fmap U.toString mQtnUuid))

findDocumentsByQuestionnaireUuidPage :: U.UUID -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page Document)
findDocumentsByQuestionnaireUuidPage qtnUuid mQuery pageable sort = do
  appUuid <- asks currentAppUuid
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "*"
    "WHERE app_uuid = ? AND name ~* ? AND questionnaire_uuid = ? AND durability='PersistentDocumentDurability'"
    (U.toString appUuid : regex mQuery : [U.toString qtnUuid])

findDocumentsByDocumentTemplateId :: String -> AppContextM [Document]
findDocumentsByDocumentTemplateId documentTemplateId = do
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("document_template_id", documentTemplateId)]

findDocumentByUuid :: U.UUID -> AppContextM Document
findDocumentByUuid uuid = do
  appUuid <- asks currentAppUuid
  createFindEntityByFn entityName [appQueryUuid appUuid, ("uuid", U.toString uuid)]

countDocuments :: AppContextM Int
countDocuments = do
  appUuid <- asks currentAppUuid
  countDocumentsWithApp appUuid

countDocumentsWithApp :: U.UUID -> AppContextM Int
countDocumentsWithApp appUuid = createCountByFn entityName appCondition [U.toString appUuid]

sumDocumentFileSize :: AppContextM Int64
sumDocumentFileSize = do
  appUuid <- asks currentAppUuid
  sumDocumentFileSizeWithApp appUuid

sumDocumentFileSizeWithApp :: U.UUID -> AppContextM Int64
sumDocumentFileSizeWithApp appUuid = createSumByFn entityName "file_size" appCondition [U.toString appUuid]

insertDocument :: Document -> AppContextM Int64
insertDocument = createInsertFn entityName

deleteDocuments :: AppContextM Int64
deleteDocuments = createDeleteEntitiesFn entityName

deleteDocumentsFiltered :: [(String, String)] -> AppContextM Int64
deleteDocumentsFiltered params = do
  appUuid <- asks currentAppUuid
  createDeleteEntitiesByFn entityName (appQueryUuid appUuid : params)

deleteDocumentByUuid :: U.UUID -> AppContextM Int64
deleteDocumentByUuid uuid = do
  appUuid <- asks currentAppUuid
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("uuid", U.toString uuid)]

deleteTemporalDocumentsByQuestionnaireUuid :: U.UUID -> AppContextM Int64
deleteTemporalDocumentsByQuestionnaireUuid qtnUuid = do
  appUuid <- asks currentAppUuid
  deleteDocumentsFiltered
    [appQueryUuid appUuid, ("questionnaire_uuid", U.toString qtnUuid), ("durability", "TemporallyDocumentDurability")]

deleteTemporalDocumentsByDocumentTemplateId :: String -> AppContextM Int64
deleteTemporalDocumentsByDocumentTemplateId documentTemplateId = do
  appUuid <- asks currentAppUuid
  deleteDocumentsFiltered
    [appQueryUuid appUuid, ("document_template_id", documentTemplateId), ("durability", "TemporallyDocumentDurability")]

deleteTemporalDocumentsByAssetUuid :: U.UUID -> AppContextM Int64
deleteTemporalDocumentsByAssetUuid = deleteTemporalDocumentsByTableAndUuid "document_template_asset"

deleteTemporalDocumentsByFileUuid :: U.UUID -> AppContextM Int64
deleteTemporalDocumentsByFileUuid = deleteTemporalDocumentsByTableAndUuid "document_template_file"

-- --------------------------------
-- PRIVATE
-- --------------------------------
deleteTemporalDocumentsByTableAndUuid :: String -> U.UUID -> AppContextM Int64
deleteTemporalDocumentsByTableAndUuid joinTableName entityUuid = do
  appUuid <- asks currentAppUuid
  let sql =
        fromString $
          f'
            "DELETE \
            \FROM document \
            \WHERE app_uuid = ? AND uuid IN ( \
            \    SELECT d.uuid \
            \    FROM %s join_table \
            \             JOIN document d ON join_table.document_template_id = d.document_template_id \
            \    WHERE join_table.uuid = '%s' \
            \      AND d.durability = 'TemporallyDocumentDurability' \
            \)"
            [joinTableName, U.toString entityUuid]
  let params = [toField appUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
