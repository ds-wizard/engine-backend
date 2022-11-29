module Wizard.Database.DAO.Document.DocumentDAO where

import Control.Monad.Reader (asks)
import Data.Maybe
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
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
findDocuments = do
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findDocumentsFiltered :: [(String, String)] -> AppContextM [Document]
findDocumentsFiltered params = do
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName (appQueryUuid appUuid : params)

findDocumentsPage :: Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page Document)
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
    (U.toString appUuid : regex mQuery : maybeToList mQtnUuid)

findDocumentsByQuestionnaireUuidPage :: String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page Document)
findDocumentsByQuestionnaireUuidPage qtnUuid mQuery pageable sort = do
  appUuid <- asks currentAppUuid
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "*"
    "WHERE app_uuid = ? AND name ~* ? AND questionnaire_uuid = ? AND durability='PersistentDocumentDurability'"
    (U.toString appUuid : regex mQuery : [qtnUuid])

findDocumentsByTemplateId :: String -> AppContextM [Document]
findDocumentsByTemplateId templateId = do
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("template_id", templateId)]

findDocumentById :: String -> AppContextM Document
findDocumentById uuid = do
  appUuid <- asks currentAppUuid
  createFindEntityByFn entityName [appQueryUuid appUuid, ("uuid", uuid)]

countDocuments :: AppContextM Int
countDocuments = do
  appUuid <- asks currentAppUuid
  countDocumentsWithApp (U.toString appUuid)

countDocumentsWithApp :: String -> AppContextM Int
countDocumentsWithApp appUuid = createCountByFn entityName appCondition [appUuid]

sumDocumentFileSize :: AppContextM Int64
sumDocumentFileSize = do
  appUuid <- asks currentAppUuid
  sumDocumentFileSizeWithApp (U.toString appUuid)

sumDocumentFileSizeWithApp :: String -> AppContextM Int64
sumDocumentFileSizeWithApp appUuid = createSumByFn entityName "file_size" appCondition [appUuid]

insertDocument :: Document -> AppContextM Int64
insertDocument = createInsertFn entityName

deleteDocuments :: AppContextM Int64
deleteDocuments = createDeleteEntitiesFn entityName

deleteDocumentsFiltered :: [(String, String)] -> AppContextM Int64
deleteDocumentsFiltered params = do
  appUuid <- asks currentAppUuid
  createDeleteEntitiesByFn entityName (appQueryUuid appUuid : params)

deleteDocumentById :: String -> AppContextM Int64
deleteDocumentById uuid = do
  appUuid <- asks currentAppUuid
  createDeleteEntityByFn entityName [appQueryUuid appUuid, ("uuid", uuid)]

deleteTemporalDocumentsByQuestionnaireUuid :: String -> AppContextM Int64
deleteTemporalDocumentsByQuestionnaireUuid qtnUuid = do
  appUuid <- asks currentAppUuid
  deleteDocumentsFiltered
    [appQueryUuid appUuid, ("questionnaire_uuid", qtnUuid), ("durability", "TemporallyDocumentDurability")]

deleteTemporalDocumentsByTemplateId :: String -> AppContextM Int64
deleteTemporalDocumentsByTemplateId templateId = do
  appUuid <- asks currentAppUuid
  deleteDocumentsFiltered
    [appQueryUuid appUuid, ("template_id", templateId), ("durability", "TemporallyDocumentDurability")]

deleteTemporalDocumentsByTemplateAssetId :: String -> AppContextM Int64
deleteTemporalDocumentsByTemplateAssetId = deleteTemporalDocumentsByTableAndId "template_asset"

deleteTemporalDocumentsByTemplateFileId :: String -> AppContextM Int64
deleteTemporalDocumentsByTemplateFileId = deleteTemporalDocumentsByTableAndId "template_file"

-- --------------------------------
-- PRIVATE
-- --------------------------------
deleteTemporalDocumentsByTableAndId :: String -> String -> AppContextM Int64
deleteTemporalDocumentsByTableAndId joinTableName entityUuid = do
  appUuid <- asks currentAppUuid
  let sql =
        fromString $
          f'
            "DELETE \
            \FROM document \
            \WHERE app_uuid = ? AND uuid IN ( \
            \    SELECT d.uuid \
            \    FROM %s join_table \
            \             JOIN document d ON join_table.template_id = d.template_id \
            \    WHERE join_table.uuid = '%s' \
            \      AND d.durability = 'TemporallyDocumentDurability' \
            \)"
            [joinTableName, entityUuid]
  let params = [toField appUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
