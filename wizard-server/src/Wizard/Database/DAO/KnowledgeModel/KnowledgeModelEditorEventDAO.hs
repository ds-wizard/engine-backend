module Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO where

import Control.Monad.Reader (asks)
import qualified Data.UUID as U
import GHC.Int

import Shared.Common.Model.Common.Sort
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.KnowledgeModel.Editor.KnowledgeModelEditorEvent ()
import Wizard.Database.Mapping.KnowledgeModel.Editor.KnowledgeModelEditorRawEvent ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorEvent
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorRawEvent

entityName = "knowledge_model_editor_event"

findKnowledgeModelEventsByEditorUuid :: U.UUID -> AppContextM [KnowledgeModelEditorEvent]
findKnowledgeModelEventsByEditorUuid kmEditorUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesWithFieldsBySortedFn "*" entityName [tenantQueryUuid tenantUuid, ("editor_uuid", U.toString kmEditorUuid)] [Sort "created_at" Ascending]

findKnowledgeModelRawEventsByEditorUuid :: U.UUID -> AppContextM [KnowledgeModelEditorRawEvent]
findKnowledgeModelRawEventsByEditorUuid kmEditorUuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesWithFieldsBySortedFn "*" entityName [tenantQueryUuid tenantUuid, ("editor_uuid", U.toString kmEditorUuid)] [Sort "created_at" Ascending]

insertKnowledgeModelEvent :: KnowledgeModelEditorEvent -> AppContextM Int64
insertKnowledgeModelEvent = createInsertFn entityName

insertKnowledgeModelRawEvent :: KnowledgeModelEditorRawEvent -> AppContextM Int64
insertKnowledgeModelRawEvent = createInsertFn entityName

deleteKnowledgeModelEventsByEditorUuid :: U.UUID -> AppContextM Int64
deleteKnowledgeModelEventsByEditorUuid kmEditorUuid = createDeleteEntityByFn entityName [("editor_uuid", U.toString kmEditorUuid)]
