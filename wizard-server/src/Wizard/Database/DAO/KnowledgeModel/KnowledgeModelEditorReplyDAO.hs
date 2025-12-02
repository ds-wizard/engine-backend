module Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorReplyDAO where

import Control.Monad.Reader (asks)
import qualified Data.List as L
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Model.Common.Sort
import Shared.Common.Util.Logger
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.KnowledgeModel.Editor.KnowledgeModelEditorReply ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorReply

entityName = "knowledge_model_editor_reply"

findKnowledgeModelRepliesByEditorUuid :: U.UUID -> AppContextM [KnowledgeModelEditorReply]
findKnowledgeModelRepliesByEditorUuid kmEditorUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesWithFieldsBySortedFn "*" entityName [tenantQueryUuid tenantUuid, ("editor_uuid", U.toString kmEditorUuid)] [Sort "created_at" Descending]

insertKnowledgeModelReply :: KnowledgeModelEditorReply -> AppContextM Int64
insertKnowledgeModelReply = createInsertFn entityName

updateKnowledgeModelRepliesByEditorUuid :: U.UUID -> [KnowledgeModelEditorReply] -> AppContextM Int64
updateKnowledgeModelRepliesByEditorUuid editorUuid editorReplies = do
  case editorReplies of
    [] -> do
      tenantUuid <- asks currentTenantUuid
      createDeleteEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("editor_uuid", U.toString editorUuid)]
    (editorReply : _) -> do
      tenantUuid <- asks currentTenantUuid
      let insertSql = L.intercalate "," . fmap (const "(?, ?, ?, ?, ?, ?, ?, ?, ?)") $ editorReplies
      let sql =
            fromString $
              f'
                "BEGIN TRANSACTION; \
                \DELETE FROM knowledge_model_editor_reply WHERE tenant_uuid = ? AND editor_uuid = ?; \
                \INSERT INTO knowledge_model_editor_reply (path, value_type, value, value_id, value_raw, editor_uuid, created_by, tenant_uuid, created_at) \
                \VALUES %s; \
                \COMMIT;"
                [insertSql]
      let params =
            [toField editorReply.tenantUuid, toField editorReply.knowledgeModelEditorUuid]
              ++ concatMap toRow editorReplies
      logInsertAndUpdate sql params
      let action conn = execute conn sql params
      runDB action

deleteKnowledgeModelRepliesByEditorUuid :: U.UUID -> AppContextM Int64
deleteKnowledgeModelRepliesByEditorUuid kmEditorUuid = createDeleteEntityByFn entityName [("editor_uuid", U.toString kmEditorUuid)]
