module Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Util.Logger
import Shared.Common.Util.String (trim)
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.KnowledgeModel.Editor.KnowledgeModelEditor ()
import Wizard.Database.Mapping.KnowledgeModel.Editor.KnowledgeModelEditorList ()
import Wizard.Database.Mapping.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion

entityName = "knowledge_model_editor"

pageLabel = "knowledgeModelEditors"

findKnowledgeModelEditors :: AppContextM [KnowledgeModelEditor]
findKnowledgeModelEditors = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findKnowledgeModelEditorsByPreviousPackageId :: String -> AppContextM [KnowledgeModelEditor]
findKnowledgeModelEditorsByPreviousPackageId previousPackageId = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("previous_package_id", previousPackageId)]

findKnowledgeModelEditorsPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page KnowledgeModelEditorList)
findKnowledgeModelEditorsPage mQuery pageable sort =
  -- 1. Prepare variables
  do
    tenantUuid <- asks currentTenantUuid
    let condition = "WHERE (name ~* ? OR km_id ~* ?) AND tenant_uuid = ?"
    let conditionParams = [regexM mQuery, regexM mQuery, U.toString tenantUuid]
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    -- 2. Get total count
    count <- createCountByFn entityName condition conditionParams
    -- 3. Get entities
    let sql =
          fromString $
            f'
              "SELECT knowledge_model_editor.uuid, \
              \       knowledge_model_editor.name, \
              \       knowledge_model_editor.km_id, \
              \       knowledge_model_editor.version, \
              \       get_knowledge_model_editor_state(knowledge_model_editor.uuid, knowledge_model_migration, get_knowledge_model_editor_fork_of_package_id(config_organization, previous_pkg, knowledge_model_editor), '%s') as state, \
              \       knowledge_model_editor.previous_package_id, \
              \       get_knowledge_model_editor_fork_of_package_id(config_organization, previous_pkg, knowledge_model_editor) as fork_of_package_id, \
              \       knowledge_model_editor.created_by, \
              \       knowledge_model_editor.created_at, \
              \       knowledge_model_editor.updated_at  \
              \FROM knowledge_model_editor \
              \         JOIN config_organization ON knowledge_model_editor.tenant_uuid = config_organization.tenant_uuid \
              \         LEFT JOIN knowledge_model_migration ON knowledge_model_editor.uuid = knowledge_model_migration.editor_uuid \
              \         LEFT JOIN knowledge_model_package previous_pkg \
              \                   ON knowledge_model_editor.previous_package_id = previous_pkg.id and knowledge_model_editor.tenant_uuid = previous_pkg.tenant_uuid \
              \WHERE (knowledge_model_editor.name ~* ? OR knowledge_model_editor.km_id ~* ?) AND knowledge_model_editor.tenant_uuid = ? \
              \%s OFFSET %s LIMIT %s"
              [U.toString tenantUuid, mapSort sort, show skip, show sizeI]
    logQuery sql conditionParams
    let action conn = query conn sql conditionParams
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

findEditorsByUnsupportedMetamodelVersion :: Int -> AppContextM [KnowledgeModelEditor]
findEditorsByUnsupportedMetamodelVersion metamodelVersion = do
  tenantUuid <- asks (.tenantUuid')
  let sql = fromString "SELECT * FROM knowledge_model_editor WHERE metamodel_version != ? AND tenant_uuid = ?"
  let params = [toField metamodelVersion, toField tenantUuid]
  logQuery sql params
  let action conn = query conn sql params
  runDB action

findEditorsForSquashing :: AppContextM [U.UUID]
findEditorsForSquashing = do
  let sql = "SELECT uuid FROM knowledge_model_editor WHERE squashed = false"
  logInfoI _CMP_DATABASE (trim sql)
  let action conn = query_ conn (fromString sql)
  entities <- runDB action
  return . concat $ entities

findKnowledgeModelEditorByUuidForSquashingLocked :: U.UUID -> AppContextM KnowledgeModelEditor
findKnowledgeModelEditorByUuidForSquashingLocked editorUuid = createFindEntityWithFieldsByFn "*" True entityName [("uuid", U.toString editorUuid)]

findKnowledgeModelEditorSuggestionsPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page KnowledgeModelEditorSuggestion)
findKnowledgeModelEditorSuggestionsPage mQuery pageable sort = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "uuid, name"
    "WHERE name ~* ? AND tenant_uuid = ?"
    [regexM mQuery, U.toString tenantUuid]

findKnowledgeModelEditorByUuid :: U.UUID -> AppContextM KnowledgeModelEditor
findKnowledgeModelEditorByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findKnowledgeModelEditorSuggestionByUuid' :: U.UUID -> AppContextM (Maybe KnowledgeModelEditorSuggestion)
findKnowledgeModelEditorSuggestionByUuid' uuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityWithFieldsByFn' "uuid, name" entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

countKnowledgeModelEditors :: AppContextM Int
countKnowledgeModelEditors = do
  tenantUuid <- asks currentTenantUuid
  countKnowledgeModelEditorsWithTenant tenantUuid

countKnowledgeModelEditorsWithTenant :: U.UUID -> AppContextM Int
countKnowledgeModelEditorsWithTenant tenantUuid = createCountByFn entityName tenantCondition [U.toString tenantUuid]

insertKnowledgeModelEditor :: KnowledgeModelEditor -> AppContextM Int64
insertKnowledgeModelEditor = createInsertFn entityName

updateKnowledgeModelEditorByUuid :: KnowledgeModelEditor -> AppContextM Int64
updateKnowledgeModelEditorByUuid kmEditor = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "UPDATE knowledge_model_editor SET uuid = ?, name = ?, km_id = ?, previous_package_id = ?, created_by = ?, created_at = ?, updated_at = ?, tenant_uuid = ?, version = ?, description = ?, readme = ?, license = ?, metamodel_version = ?, squashed = ? WHERE tenant_uuid = ? AND uuid = ?;"
  let params = toRow kmEditor ++ [toField tenantUuid, toField . U.toText $ kmEditor.uuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

clearKnowledgeModelEditorCreatedBy :: U.UUID -> AppContextM ()
clearKnowledgeModelEditorCreatedBy userUuid = do
  let sql = fromString "UPDATE knowledge_model_editor SET created_by = null WHERE created_by = ?"
  let params = [toField userUuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()

deleteKnowledgeModelEditors :: AppContextM Int64
deleteKnowledgeModelEditors = createDeleteEntitiesFn entityName

deleteKnowledgeModelEditorByUuid :: U.UUID -> AppContextM Int64
deleteKnowledgeModelEditorByUuid uuid = createDeleteEntityByFn entityName [("uuid", U.toString uuid)]
