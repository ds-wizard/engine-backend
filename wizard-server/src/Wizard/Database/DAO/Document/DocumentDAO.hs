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
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findDocumentsFiltered :: [(String, String)] -> AppContextM [Document]
findDocumentsFiltered params = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName (tenantQueryUuid tenantUuid : params)

findDocumentsPage :: Maybe U.UUID -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page Document)
findDocumentsPage mQtnUuid mQuery pageable sort = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "*"
    ( if isJust mQtnUuid
        then "WHERE tenant_uuid = ? AND name ~* ? AND questionnaire_uuid = ? AND durability='PersistentDocumentDurability'"
        else "WHERE tenant_uuid = ? AND name ~* ? AND durability='PersistentDocumentDurability'"
    )
    (U.toString tenantUuid : regex mQuery : maybeToList (fmap U.toString mQtnUuid))

findDocumentsByQuestionnaireUuidPage :: U.UUID -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page Document)
findDocumentsByQuestionnaireUuidPage qtnUuid mQuery pageable sort = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "*"
    "WHERE tenant_uuid = ? AND name ~* ? AND questionnaire_uuid = ? AND durability='PersistentDocumentDurability'"
    (U.toString tenantUuid : regex mQuery : [U.toString qtnUuid])

findDocumentsByDocumentTemplateId :: String -> AppContextM [Document]
findDocumentsByDocumentTemplateId documentTemplateId = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_id", documentTemplateId)]

findDocumentByUuid :: U.UUID -> AppContextM Document
findDocumentByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

countDocuments :: AppContextM Int
countDocuments = do
  tenantUuid <- asks currentTenantUuid
  countDocumentsWithTenant tenantUuid

countDocumentsWithTenant :: U.UUID -> AppContextM Int
countDocumentsWithTenant tenantUuid = createCountByFn entityName tenantCondition [U.toString tenantUuid]

sumDocumentFileSize :: AppContextM Int64
sumDocumentFileSize = do
  tenantUuid <- asks currentTenantUuid
  sumDocumentFileSizeWithTenant tenantUuid

sumDocumentFileSizeWithTenant :: U.UUID -> AppContextM Int64
sumDocumentFileSizeWithTenant tenantUuid = createSumByFn entityName "file_size" tenantCondition [U.toString tenantUuid]

insertDocument :: Document -> AppContextM Int64
insertDocument = createInsertFn entityName

deleteDocuments :: AppContextM Int64
deleteDocuments = createDeleteEntitiesFn entityName

deleteDocumentsFiltered :: [(String, String)] -> AppContextM Int64
deleteDocumentsFiltered params = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntitiesByFn entityName (tenantQueryUuid tenantUuid : params)

deleteDocumentByUuid :: U.UUID -> AppContextM Int64
deleteDocumentByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

deleteTemporalDocumentsByQuestionnaireUuid :: U.UUID -> AppContextM Int64
deleteTemporalDocumentsByQuestionnaireUuid qtnUuid = do
  tenantUuid <- asks currentTenantUuid
  deleteDocumentsFiltered
    [tenantQueryUuid tenantUuid, ("questionnaire_uuid", U.toString qtnUuid), ("durability", "TemporallyDocumentDurability")]

deleteTemporalDocumentsByDocumentTemplateId :: String -> AppContextM Int64
deleteTemporalDocumentsByDocumentTemplateId documentTemplateId = do
  tenantUuid <- asks currentTenantUuid
  deleteDocumentsFiltered
    [tenantQueryUuid tenantUuid, ("document_template_id", documentTemplateId), ("durability", "TemporallyDocumentDurability")]

deleteTemporalDocumentsByAssetUuid :: U.UUID -> AppContextM Int64
deleteTemporalDocumentsByAssetUuid = deleteTemporalDocumentsByTableAndUuid "document_template_asset"

deleteTemporalDocumentsByFileUuid :: U.UUID -> AppContextM Int64
deleteTemporalDocumentsByFileUuid = deleteTemporalDocumentsByTableAndUuid "document_template_file"

-- --------------------------------
-- PRIVATE
-- --------------------------------
deleteTemporalDocumentsByTableAndUuid :: String -> U.UUID -> AppContextM Int64
deleteTemporalDocumentsByTableAndUuid joinTableName entityUuid = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString $
          f'
            "DELETE \
            \FROM document \
            \WHERE tenant_uuid = ? AND uuid IN ( \
            \    SELECT d.uuid \
            \    FROM %s join_table \
            \             JOIN document d ON join_table.document_template_id = d.document_template_id \
            \    WHERE join_table.uuid = '%s' \
            \      AND d.durability = 'TemporallyDocumentDurability' \
            \)"
            [joinTableName, U.toString entityUuid]
  let params = [toField tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
