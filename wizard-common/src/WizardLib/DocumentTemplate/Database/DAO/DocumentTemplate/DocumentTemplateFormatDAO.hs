module WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO where

import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.String
import WizardLib.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplateFormat ()
import WizardLib.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplateFormatSimple ()
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple

findDocumentTemplateFormats :: AppContextC s sc m => String -> m [DocumentTemplateFormat]
findDocumentTemplateFormats documentTemplateId = do
  tenantUuid <- asks (.tenantUuid')
  formats <- createFindEntitiesBySortedFn "document_template_format" [tenantQueryUuid tenantUuid, ("document_template_id", documentTemplateId)] [Sort "name" Ascending]
  traverse
    ( \format -> do
        steps <- findDocumentTemplateFormatSteps documentTemplateId format.uuid
        return $ format {steps = steps}
    )
    formats

findDocumentTemplateFormatSteps :: AppContextC s sc m => String -> U.UUID -> m [DocumentTemplateFormatStep]
findDocumentTemplateFormatSteps documentTemplateId formatUuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntitiesByFn "document_template_format_step" [("tenant_uuid", U.toString tenantUuid), ("document_template_id", documentTemplateId), ("format_uuid", U.toString formatUuid)]

-- findFormatListsByDocumentTemplateId :: AppContextC s sc m => String -> m [DocumentTemplateFormatList]
-- findFormatListsByDocumentTemplateId documentTemplateId = do
--   tenantUuid <- asks (.tenantUuid')
--   createFindEntitiesWithFieldsByFn "uuid, format_name, created_at, updated_at" entityName [tenantQueryUuid tenantUuid, ("document_template_id", documentTemplateId)]

-- findFormatsByDocumentTemplateIdAndFormatName :: AppContextC s sc m => String -> String -> m [DocumentTemplateFormat]
-- findFormatsByDocumentTemplateIdAndFormatName documentTemplateId formatName = do
--   tenantUuid <- asks (.tenantUuid')
--   createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_id", documentTemplateId), ("format_name", formatName)]

findDocumentTemplateFormatByDocumentTemplateIdAndUuid :: AppContextC s sc m => String -> U.UUID -> m DocumentTemplateFormatSimple
findDocumentTemplateFormatByDocumentTemplateIdAndUuid documentTemplateId uuid = do
  tenantUuid <- asks (.tenantUuid')
  createFindEntityWithFieldsByFn "uuid, name, icon" False "document_template_format" [tenantQueryUuid tenantUuid, ("document_template_id", documentTemplateId), ("uuid", U.toString uuid)]

insertDocumentTemplateFormat :: AppContextC s sc m => DocumentTemplateFormat -> m ()
insertDocumentTemplateFormat format = do
  createInsertFn "document_template_format" format
  traverse_ (createInsertFn "document_template_format_step") format.steps

insertOrUpdateDocumentTemplateFormat :: AppContextC s sc m => DocumentTemplateFormat -> m Int64
insertOrUpdateDocumentTemplateFormat format = do
  let sql =
        fromString $
          "INSERT INTO document_template_format \
          \VALUES (?, ?, ?, ?, ?, ?, ?) \
          \ON CONFLICT (tenant_uuid, document_template_id, uuid) DO UPDATE SET document_template_id = ?, \
          \                                                                    uuid                 = ?, \
          \                                                                    name                 = ?, \
          \                                                                    icon                 = ?, \
          \                                                                    tenant_uuid          = ?, \
          \                                                                    created_at           = ?, \
          \                                                                    updated_at           = ?; \
          \DELETE FROM document_template_format_step WHERE tenant_uuid = ? AND document_template_id = ? AND format_uuid = ?;"
            ++ concatMap (const "INSERT INTO document_template_format_step VALUES (?, ?, ?, ?, ?, ?, ?, ?);") format.steps
  let params =
        toRow format
          ++ toRow format
          ++ [toField format.tenantUuid, toField format.documentTemplateId, toField format.uuid]
          ++ concatMap toRow format.steps
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteDocumentTemplateFormatsExcept :: AppContextC s sc m => String -> [U.UUID] -> m Int64
deleteDocumentTemplateFormatsExcept documentTemplateId formatUuids = do
  let formatUuidsCondition =
        case formatUuids of
          [] -> ""
          _ -> f' "AND uuid NOT IN (%s)" [generateQuestionMarks formatUuids]
  tenantUuid <- asks (.tenantUuid')
  let sql =
        fromString $
          f'
            "DELETE FROM document_template_format \
            \WHERE tenant_uuid = ? AND document_template_id = ? %s"
            [formatUuidsCondition]
  let params = [U.toString tenantUuid, documentTemplateId] ++ fmap U.toString formatUuids
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteFormats :: AppContextC s sc m => m Int64
deleteFormats = createDeleteEntitiesFn "document_template_format"

-- deleteFormatsByDocumentTemplateId :: AppContextC s sc m => String -> m Int64
-- deleteFormatsByDocumentTemplateId tmlId = do
--   tenantUuid <- asks (.tenantUuid')
--   createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_id", tmlId)]

-- deleteFormatById :: AppContextC s sc m => U.UUID -> m Int64
-- deleteFormatById uuid = do
--   tenantUuid <- asks (.tenantUuid')
--   createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]
