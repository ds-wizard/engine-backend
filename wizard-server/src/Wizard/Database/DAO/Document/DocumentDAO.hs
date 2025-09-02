module Wizard.Database.DAO.Document.DocumentDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import GHC.Int

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Util.Logger
import Shared.Common.Util.String
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Document.Document ()
import Wizard.Database.Mapping.Document.DocumentList ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Document.Document
import Wizard.Model.Document.DocumentList

entityName = "document"

pageLabel = "documents"

findDocuments :: AppContextM [Document]
findDocuments = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findDocumentsFiltered :: [(String, String)] -> AppContextM [Document]
findDocumentsFiltered = createFindEntitiesByFn entityName

findDocumentsForCurrentTenantFiltered :: [(String, String)] -> AppContextM [Document]
findDocumentsForCurrentTenantFiltered params = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName (tenantQueryUuid tenantUuid : params)

findDocumentsPage :: Maybe U.UUID -> Maybe String -> Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page DocumentList)
findDocumentsPage mQtnUuid mQtnName mDocumentTemplateId mQuery pageable sort = do
  -- 1. Prepare variables
  do
    tenantUuid <- asks currentTenantUuid
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    let (questionnaireSelect, questionnaireSelectParams, questionnaireJoin, questionnaireCondition, questionnaireParam) =
          case (mQtnUuid, mQtnName) of
            (Just qtnUuid, Just qtnName) -> ("?, ", [qtnName], "", "AND doc.questionnaire_uuid = ?", [U.toString qtnUuid])
            (Just qtnUuid, Nothing) -> ("qtn.name, ", [], "LEFT JOIN questionnaire qtn ON qtn.uuid = doc.questionnaire_uuid", "AND doc.questionnaire_uuid = ?", [U.toString qtnUuid])
            _ -> ("qtn.name, ", [], "LEFT JOIN questionnaire qtn ON qtn.uuid = doc.questionnaire_uuid", "", [])
    let (documentTemplateIdCondition, documentTemplateIdParam) =
          case mDocumentTemplateId of
            Just documentTemplateId -> (" AND doc.document_template_id = ? ", [documentTemplateId])
            Nothing -> ("", [])
    let condition = "WHERE doc.tenant_uuid = ? AND doc.name ~* ? AND doc.durability = 'PersistentDocumentDurability' " ++ questionnaireCondition ++ documentTemplateIdCondition
    let baseParams = [U.toString tenantUuid, regexM mQuery] ++ questionnaireParam ++ documentTemplateIdParam
    let params = questionnaireSelectParams ++ baseParams
    -- 2. Get total count
    count <- createCountByFn "document doc" condition baseParams
    -- 3. Get entities
    let sql =
          fromString $
            f''
              "SELECT doc.uuid, \
              \       doc.name, \
              \       doc.state, \
              \       doc.questionnaire_uuid, \
              \       ${questionnaireSelect} \
              \       doc.questionnaire_event_uuid, \
              \       qtn_version.name, \
              \       doc_tml.id, \
              \       doc_tml.name, \
              \       ( \
              \        SELECT jsonb_agg(jsonb_build_object('uuid', uuid, 'name', name, 'icon', icon)) \
              \        FROM (SELECT * \
              \              FROM document_template_format dt_format \
              \              WHERE dt_format.tenant_uuid = doc.tenant_uuid AND dt_format.document_template_id = doc.document_template_id \
              \              ORDER BY dt_format.name) nested \
              \       ) AS document_template_formats, \
              \       doc.format_uuid, \
              \       doc.file_size, \
              \       doc.worker_log, \
              \       doc.created_by, \
              \       doc.created_at \
              \FROM document doc \
              \${questionnaireJoin} \
              \LEFT JOIN document_template doc_tml ON doc_tml.id = doc.document_template_id AND doc_tml.tenant_uuid = doc.tenant_uuid \
              \LEFT JOIN questionnaire_version qtn_version ON qtn_version.event_uuid = doc.questionnaire_event_uuid AND qtn_version.tenant_uuid = doc.tenant_uuid \
              \${condition} \
              \${sort} \
              \OFFSET ${offset} \
              \LIMIT ${limit}"
              [ ("questionnaireSelect", questionnaireSelect)
              , ("questionnaireJoin", questionnaireJoin)
              , ("condition", condition)
              , ("sort", mapSortWithPrefix "doc" sort)
              , ("offset", show skip)
              , ("limit", show sizeI)
              ]
    logQuery sql params
    let action conn = query conn sql params
    entities <- runDB action
    -- 4. Constructor response
    let metadata =
          PageMetadata
            { size = sizeI
            , totalElements = count
            , totalPages = computeTotalPage count sizeI
            , number = pageI
            }
    return $ Page pageLabel metadata entities

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

deleteDocumentByUuidAndTenantUuid :: U.UUID -> U.UUID -> AppContextM Int64
deleteDocumentByUuidAndTenantUuid uuid tenantUuid = createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

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
