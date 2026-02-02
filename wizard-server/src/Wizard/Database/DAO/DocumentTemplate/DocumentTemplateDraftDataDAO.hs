module Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import qualified Data.UUID as U
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

findDraftDataByUuid :: U.UUID -> AppContextM DocumentTemplateDraftData
findDraftDataByUuid documentTemplateUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString documentTemplateUuid)]

insertDraftData :: DocumentTemplateDraftData -> AppContextM Int64
insertDraftData = createInsertFn entityName

updateDraftDataById :: DocumentTemplateDraftData -> AppContextM Int64
updateDraftDataById draftData = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "UPDATE document_template_draft_data SET document_template_uuid = ?, project_uuid = ?, format_uuid = ?, tenant_uuid = ?, created_at = ?, updated_at = ?, knowledge_model_editor_uuid = ? WHERE tenant_uuid = ? AND document_template_uuid = ?"
  let params = toRow draftData ++ [toField draftData.tenantUuid, toField draftData.documentTemplateUuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteDraftData :: AppContextM Int64
deleteDraftData = createDeleteEntitiesFn entityName

deleteDraftDataByDocumentTemplateUuid :: U.UUID -> AppContextM Int64
deleteDraftDataByDocumentTemplateUuid dtUuid = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("document_template_uuid", U.toString dtUuid)]
