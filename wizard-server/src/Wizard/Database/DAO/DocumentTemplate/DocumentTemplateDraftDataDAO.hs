module Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.DocumentTemplate.DocumentTemplateDraftData ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftData

entityName = "document_template_draft_data"

findDraftDataById :: String -> AppContextM DocumentTemplateDraftData
findDraftDataById id = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("document_template_id", id)]

insertDraftData :: DocumentTemplateDraftData -> AppContextM Int64
insertDraftData = createInsertFn entityName

updateDraftDataById :: DocumentTemplateDraftData -> AppContextM Int64
updateDraftDataById draftData = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "UPDATE document_template_draft_data SET document_template_id = ?, questionnaire_uuid = ?, format_uuid = ?, tenant_uuid = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ? AND document_template_id = ?"
  let params = toRow draftData ++ [toField draftData.tenantUuid, toField draftData.documentTemplateId]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteDraftDatas :: AppContextM Int64
deleteDraftDatas = createDeleteEntitiesFn entityName

deleteDraftDataByDocumentTemplateId :: String -> AppContextM Int64
deleteDraftDataByDocumentTemplateId tmlId = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_id", tmlId)]
