module Wizard.Service.KnowledgeModel.Editor.EditorAudit where

import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.UUID as U

import Shared.Audit.Service.Audit.AuditService
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorEvent

auditKnowledgeModelEditorPublish :: KnowledgeModelEditor -> [KnowledgeModelEditorEvent] -> Maybe String -> AppContextM ()
auditKnowledgeModelEditorPublish kmEditor kmEditorEvents mForkOfPkgId =
  logAuditWithBody
    "knowledge_model_editor"
    "publish"
    (U.toString $ kmEditor.uuid)
    ( M.fromList
        [ ("kmId", kmEditor.kmId)
        , ("eventSize", show . length $ kmEditorEvents)
        , ("isFork", show . isJust $ mForkOfPkgId)
        ]
    )
