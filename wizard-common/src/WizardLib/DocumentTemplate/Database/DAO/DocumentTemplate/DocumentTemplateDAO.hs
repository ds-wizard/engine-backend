module WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO where

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
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateAssetDAO (deleteAssets, deleteAssetsByDocumentTemplateId)
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO (deleteFiles, deleteFilesByDocumentTemplateId)
import WizardLib.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplate ()
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

entityName = "document_template"

findDocumentTemplates :: AppContextC s sc m => m [DocumentTemplate]
findDocumentTemplates = do
  appUuid <- asks (.appUuid')
  let sql =
        fromString
          "SELECT * \
          \FROM document_template \
          \WHERE app_uuid = ? AND (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase')"
  let params = [U.toString appUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findDocumentTemplatesFiltered :: AppContextC s sc m => [(String, String)] -> m [DocumentTemplate]
findDocumentTemplatesFiltered queryParams = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn entityName (appQueryUuid appUuid : queryParams)

findDocumentTemplatesByOrganizationIdAndKmId :: AppContextC s sc m => String -> String -> m [DocumentTemplate]
findDocumentTemplatesByOrganizationIdAndKmId organizationId templateId = do
  appUuid <- asks (.appUuid')
  createFindEntitiesByFn
    entityName
    [appQueryUuid appUuid, ("organization_id", organizationId), ("template_id", templateId)]

findDocumentTemplateById :: AppContextC s sc m => String -> m DocumentTemplate
findDocumentTemplateById id = do
  appUuid <- asks (.appUuid')
  createFindEntityByFn entityName [appQueryUuid appUuid, ("id", id)]

findDocumentTemplateById' :: AppContextC s sc m => String -> m (Maybe DocumentTemplate)
findDocumentTemplateById' id = do
  appUuid <- asks (.appUuid')
  createFindEntityByFn' entityName [appQueryUuid appUuid, ("id", id)]

findVersionsForDocumentTemplate :: AppContextC s sc m => String -> String -> m [String]
findVersionsForDocumentTemplate orgId templateId = do
  appUuid <- asks (.appUuid')
  let sql = fromString "SELECT version FROM document_template WHERE app_uuid = ? and organization_id = ? and template_id = ?"
  let params = [U.toString appUuid, orgId, templateId]
  logQuery sql params
  let action conn = query conn sql params
  versions <- runDB action
  return . fmap fromOnly $ versions

countDocumentTemplatesGroupedByOrganizationIdAndKmId :: AppContextC s sc m => m Int
countDocumentTemplatesGroupedByOrganizationIdAndKmId = do
  appUuid <- asks (.appUuid')
  countDocumentTemplatesGroupedByOrganizationIdAndKmIdWithApp appUuid

countDocumentTemplatesGroupedByOrganizationIdAndKmIdWithApp :: AppContextC s sc m => U.UUID -> m Int
countDocumentTemplatesGroupedByOrganizationIdAndKmIdWithApp appUuid = do
  let sql =
        "SELECT COUNT(*) \
        \FROM (SELECT 1 \
        \      FROM document_template \
        \      WHERE app_uuid = ? AND (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase') \
        \      GROUP BY organization_id, template_id) nested;"
  let params = [U.toString appUuid]
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
  appUuid <- asks (.appUuid')
  let sql =
        fromString
          "UPDATE document_template SET id = ?, name = ?, organization_id = ?, template_id = ?, version = ?, metamodel_version = ?, description = ?, readme = ?, license = ?, allowed_packages = ?, formats = ?, created_at = ?, app_uuid = ?, updated_at = ?, phase = ? WHERE app_uuid = ? AND id = ?"
  let params = toRow tml ++ [toField appUuid, toField tml.tId]
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
  appUuid <- asks (.appUuid')
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
            \WHERE app_uuid = ? AND (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase') %s"
            [queryCondition]
  let params = U.toString appUuid : fmap snd queryParams
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteDocumentTemplateById :: AppContextC s sc m => String -> m Int64
deleteDocumentTemplateById documentTemplateId = do
  appUuid <- asks (.appUuid')
  deleteFilesByDocumentTemplateId documentTemplateId
  deleteAssetsByDocumentTemplateId documentTemplateId
  let sql =
        fromString
          "DELETE FROM document_template \
          \WHERE id = ? AND app_uuid = ? AND (phase = 'ReleasedDocumentTemplatePhase' OR phase = 'DeprecatedDocumentTemplatePhase')"
  let params = [documentTemplateId, U.toString appUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
