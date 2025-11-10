module Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO where

import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.String
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO (deleteAssets, deleteAssetsByDocumentTemplateId)
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO (deleteFiles, deleteFilesByDocumentTemplateId)
import Shared.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplate ()
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

entityName = "document_template"

findDocumentTemplates :: AppContextC s sc m => m [DocumentTemplate]
findDocumentTemplates = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "SELECT * \
          \FROM document_template \
          \WHERE tenant_uuid = ? AND (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase')"
  let params = [U.toString tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findDocumentTemplatesFiltered :: AppContextC s sc m => [(String, String)] -> m [DocumentTemplate]
findDocumentTemplatesFiltered queryParams = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn entityName (tenantQueryUuid tenantUuid : queryParams)

findDocumentTemplatesByOrganizationIdAndKmId :: AppContextC s sc m => String -> String -> m [DocumentTemplate]
findDocumentTemplatesByOrganizationIdAndKmId organizationId templateId = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn
    entityName
    [tenantQueryUuid tenantUuid, ("organization_id", organizationId), ("template_id", templateId)]

findDocumentTemplateById :: AppContextC s sc m => String -> m DocumentTemplate
findDocumentTemplateById id = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("id", id)]

findDocumentTemplateById' :: AppContextC s sc m => String -> m (Maybe DocumentTemplate)
findDocumentTemplateById' id = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("id", id)]

findVersionsForDocumentTemplate :: AppContextC s sc m => String -> String -> m [String]
findVersionsForDocumentTemplate orgId templateId = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "SELECT version FROM document_template WHERE tenant_uuid = ? and organization_id = ? and template_id = ?"
  let params = [U.toString tenantUuid, orgId, templateId]
  logQuery sql params
  let action conn = query conn sql params
  versions <- runDB action
  return . fmap fromOnly $ versions

countDocumentTemplatesGroupedByOrganizationIdAndKmId :: AppContextC s sc m => m Int
countDocumentTemplatesGroupedByOrganizationIdAndKmId = do
  tenantUuid <- asks (.tenantUuid')
  countDocumentTemplatesGroupedByOrganizationIdAndKmIdWithTenant tenantUuid

countDocumentTemplatesGroupedByOrganizationIdAndKmIdWithTenant :: AppContextC s sc m => U.UUID -> m Int
countDocumentTemplatesGroupedByOrganizationIdAndKmIdWithTenant tenantUuid = do
  let sql =
        "SELECT COUNT(*) \
        \FROM (SELECT 1 \
        \      FROM document_template \
        \      WHERE tenant_uuid = ? AND (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase') \
        \      GROUP BY organization_id, template_id) nested;"
  let params = [U.toString tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

insertDocumentTemplate :: AppContextC s sc m => DocumentTemplate -> m Int64
insertDocumentTemplate = createInsertFn entityName

updateDocumentTemplateById :: AppContextC s sc m => DocumentTemplate -> m Int64
updateDocumentTemplateById tml = do
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString
          "UPDATE document_template SET id = ?, name = ?, organization_id = ?, template_id = ?, version = ?, metamodel_version = ?, description = ?, readme = ?, license = ?, allowed_packages = ?, created_at = ?, tenant_uuid = ?, updated_at = ?, phase = ?, non_editable = ? WHERE tenant_uuid = ? AND id = ?"
  let params = toRow tml ++ [toField tenantUuid, toField tml.tId]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteDocumentTemplates :: AppContextC s sc m => m Int64
deleteDocumentTemplates = do
  deleteFiles
  deleteAssets
  createDeleteEntitiesFn entityName

deleteDocumentTemplatesFiltered :: AppContextC s sc m => [(String, String)] -> m Int64
deleteDocumentTemplatesFiltered queryParams = do
  tenantUuid <- asks (.tenantUuid')
  templates <- findDocumentTemplatesFiltered queryParams
  traverse_ (\t -> deleteFilesByDocumentTemplateId t.tId) templates
  traverse_ (\t -> deleteAssetsByDocumentTemplateId t.tId) templates
  let queryCondition =
        case queryParams of
          [] -> ""
          _ -> f' "AND %s" [mapToDBQuerySql queryParams]
  let sql =
        fromString $
          f'
            "DELETE FROM document_template \
            \WHERE tenant_uuid = ? AND (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase') %s"
            [queryCondition]
  let params = U.toString tenantUuid : fmap snd queryParams
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteDocumentTemplateById :: AppContextC s sc m => String -> m Int64
deleteDocumentTemplateById documentTemplateId = do
  tenantUuid <- asks (.tenantUuid')
  deleteFilesByDocumentTemplateId documentTemplateId
  deleteAssetsByDocumentTemplateId documentTemplateId
  let sql =
        fromString
          "DELETE FROM document_template \
          \WHERE id = ? AND tenant_uuid = ? AND (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase')"
  let params = [documentTemplateId, U.toString tenantUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
