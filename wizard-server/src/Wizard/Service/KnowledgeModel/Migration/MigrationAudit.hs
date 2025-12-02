module Wizard.Service.KnowledgeModel.Migration.MigrationAudit where

import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.UUID as U

import Shared.Audit.Service.Audit.AuditService
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationCreateDTO
import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationResolutionDTO
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor

auditKmMigrationCreate :: KnowledgeModelMigrationCreateDTO -> KnowledgeModelEditor -> AppContextM ()
auditKmMigrationCreate reqDto kmEditor =
  logAuditWithBody
    "knowledge_model.migration"
    "create"
    (U.toString kmEditor.uuid)
    ( M.fromList
        [("sourcePackageId", fromMaybe "" $ kmEditor.previousPackageId), ("targetPackageId", reqDto.targetPackageId)]
    )

auditKmMigrationSolve :: U.UUID -> KnowledgeModelMigrationResolutionDTO -> AppContextM ()
auditKmMigrationSolve editorUuid reqDto =
  logAuditWithBody
    "knowledge_model.migration"
    "solve"
    (U.toString editorUuid)
    (M.fromList [("originalEventUuid", U.toString $ reqDto.originalEventUuid), ("action", show reqDto.action)])

auditKmMigrationApplyAll :: U.UUID -> AppContextM ()
auditKmMigrationApplyAll editorUuid = logAudit "knowledge_model.migration" "applyAll" (U.toString editorUuid)

auditKmMigrationCancel :: U.UUID -> AppContextM ()
auditKmMigrationCancel editorUuid = logAudit "knowledge_model.migration" "cancel" (U.toString editorUuid)

auditKmMigrationFinish :: U.UUID -> AppContextM ()
auditKmMigrationFinish editorUuid = logAudit "knowledge_model.migration" "finish" (U.toString editorUuid)
