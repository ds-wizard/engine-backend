module Wizard.Database.DAO.QuestionnaireAction.QuestionnaireActionDAO where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common hiding (runInTransaction)
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Shared.Common.Util.Logger
import Wizard.Database.Mapping.QuestionnaireAction.QuestionnaireAction ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.QuestionnaireAction.QuestionnaireAction

entityName = "questionnaire_action"

pageLabel = "questionnaireActions"

findQuestionnaireActionsPage
  :: Maybe String
  -> Maybe String
  -> Maybe String
  -> Maybe Bool
  -> Pageable
  -> [Sort]
  -> AppContextM (Page QuestionnaireAction)
findQuestionnaireActionsPage mOrganizationId mImporterId mQuery mEnabled pageable sort =
  createFindEntitiesGroupByCoordinatePageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "*"
    "action_id"
    mQuery
    mEnabled
    mOrganizationId
    mImporterId

findQuestionnaireActionById :: String -> AppContextM QuestionnaireAction
findQuestionnaireActionById qaId = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("id", qaId)]

insertQuestionnaireAction :: QuestionnaireAction -> AppContextM Int64
insertQuestionnaireAction = createInsertFn entityName

updateQuestionnaireActionById :: QuestionnaireAction -> AppContextM Int64
updateQuestionnaireActionById importer = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "UPDATE questionnaire_action SET id = ?, name = ?, organization_id = ?, action_id = ?, version = ?, metamodel_version = ?, description = ?, readme = ?, license = ?, allowed_packages = ?, url = ?, config = ?, enabled = ?, tenant_uuid = ?, created_at = ?, updated_at = ? WHERE tenant_uuid = ? AND id = ?"
  let params = toRow importer ++ [toField tenantUuid, toField importer.qaId]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

updateQuestionnaireActionPasswordById :: String -> Bool -> UTCTime -> AppContextM Int64
updateQuestionnaireActionPasswordById qaId enabled uUpdatedAt = do
  tenantUuid <- asks currentTenantUuid
  let sql = fromString "UPDATE questionnaire_action SET enabled = ?, updated_at = ? WHERE tenant_uuid = ? AND uuid = ?"
  let params = [toField enabled, toField uUpdatedAt, toField tenantUuid, toField qaId]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action

deleteQuestionnaireActions :: AppContextM Int64
deleteQuestionnaireActions = createDeleteEntitiesFn entityName

deleteQuestionnaireActionById :: String -> AppContextM Int64
deleteQuestionnaireActionById qaId = do
  tenantUuid <- asks currentTenantUuid
  createDeleteEntityByFn entityName [tenantQueryUuid tenantUuid, ("id", qaId)]

-- --------------------------------
-- PRIVATE
-- --------------------------------
createFindEntitiesGroupByCoordinatePageableQuerySortFn entityName pageLabel pageable sort fields entityId mQuery mEnabled mOrganizationId mEntityId =
  -- 1. Prepare variables
  do
    tenantUuid <- asks currentTenantUuid
    let (sizeI, pageI, skip, limit) = preparePaginationVariables pageable
    let enabledCondition =
          case mEnabled of
            Just True -> "enabled = true AND"
            Just False -> "enabled = false AND"
            _ -> ""
    -- 2. Get total count
    count <- createCountGroupByCoordinateFn entityName entityId mQuery enabledCondition mOrganizationId mEntityId
    -- 3. Get entities
    let sql =
          f'
            "SELECT %s \
            \FROM %s \
            \WHERE tenant_uuid = ? AND id IN ( \
            \    SELECT CONCAT(organization_id, ':', %s, ':', (max(string_to_array(version, '.')::int[]))[1] || '.' || \
            \                                                    (max(string_to_array(version, '.')::int[]))[2] || '.' || \
            \                                                    (max(string_to_array(version, '.')::int[]))[3]) \
            \    FROM %s \
            \    WHERE %s tenant_uuid = ? AND (name ~* ? OR id ~* ?) %s \
            \    GROUP BY organization_id, %s \
            \) \
            \%s \
            \offset %s \
            \limit %s"
            [ fields
            , entityName
            , entityId
            , entityName
            , enabledCondition
            , mapToDBCoordinatesSql entityName entityId mOrganizationId mEntityId
            , entityId
            , mapSort sort
            , show skip
            , show sizeI
            ]
    logInfo _CMP_DATABASE sql
    let action conn =
          query
            conn
            (fromString sql)
            ( U.toString tenantUuid
                : U.toString tenantUuid
                : regexM mQuery
                : regexM mQuery
                : mapToDBCoordinatesParams mOrganizationId mEntityId
            )
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

createCountGroupByCoordinateFn
  :: String -> String -> Maybe String -> String -> Maybe String -> Maybe String -> AppContextM Int
createCountGroupByCoordinateFn entityName entityId mQuery enabledCondition mOrganizationId mEntityId = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        f'
          "SELECT COUNT(*) \
          \ FROM (SELECT COUNT(*) \
          \   FROM %s \
          \   WHERE %s tenant_uuid = ? AND (name ~* ? OR id ~* ?) %s \
          \   GROUP BY organization_id, %s) p"
          [entityName, enabledCondition, mapToDBCoordinatesSql entityName entityId mOrganizationId mEntityId, entityId]
  logInfo _CMP_DATABASE sql
  let action conn =
        query
          conn
          (fromString sql)
          (U.toString tenantUuid : regexM mQuery : regexM mQuery : mapToDBCoordinatesParams mOrganizationId mEntityId)
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0
