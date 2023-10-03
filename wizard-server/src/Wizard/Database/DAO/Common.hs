module Wizard.Database.DAO.Common (
  module Shared.Common.Database.DAO.Common,
  runInTransaction,
  createFindEntitiesGroupByCoordinatePageableQuerySortFn,
  createCountGroupByCoordinateFn,
) where

import Control.Monad.Reader (asks)
import Data.Maybe (maybeToList)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple

import Shared.Common.Database.DAO.Common hiding (runInTransaction)
import qualified Shared.Common.Database.DAO.Common as S
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Util.Logger
import Shared.Common.Util.String
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import WizardLib.KnowledgeModel.Database.Mapping.Package.Package ()

runInTransaction :: AppContextM a -> AppContextM a
runInTransaction = S.runInTransaction logInfoI logWarnI

createFindEntitiesGroupByCoordinatePageableQuerySortFn entityName pageLabel pageable sort fields entityId mQuery mEnabled mOrganizationId mEntityId mState stateCondition =
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
    count <-
      createCountGroupByCoordinateFn
        entityName
        entityId
        mQuery
        enabledCondition
        mOrganizationId
        mEntityId
        mState
        stateCondition
    -- 3. Get entities
    let sql =
          f'
            "SELECT %s \
            \FROM %s \
            \LEFT JOIN registry_%s ON %s.organization_id = registry_%s.organization_id AND %s.%s = registry_%s.%s \
            \LEFT JOIN registry_organization ON %s.organization_id = registry_organization.organization_id \
            \WHERE tenant_uuid = ? AND id IN ( \
            \    SELECT CONCAT(organization_id, ':', %s, ':', (max(string_to_array(version, '.')::int[]))[1] || '.' || \
            \                                                    (max(string_to_array(version, '.')::int[]))[2] || '.' || \
            \                                                    (max(string_to_array(version, '.')::int[]))[3]) \
            \    FROM %s \
            \    WHERE %s tenant_uuid = ? AND (name ~* ? OR id ~* ?) %s \
            \    GROUP BY organization_id, %s \
            \) \
            \%s \
            \%s \
            \offset %s \
            \limit %s"
            [ fields
            , entityName
            , entityName
            , entityName
            , entityName
            , entityName
            , entityId
            , entityName
            , entityId
            , entityName
            , entityId
            , entityName
            , enabledCondition
            , mapToDBCoordinatesSql entityName entityId mOrganizationId mEntityId
            , entityId
            , stateCondition
            , mapSort sort
            , show skip
            , show sizeI
            ]
    logInfoI _CMP_DATABASE (trim sql)
    let action conn =
          query
            conn
            (fromString sql)
            ( U.toString tenantUuid
                : U.toString tenantUuid
                : regex mQuery
                : regex mQuery
                : mapToDBCoordinatesParams mOrganizationId mEntityId
                ++ maybeToList mState
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
  :: String
  -> String
  -> Maybe String
  -> String
  -> Maybe String
  -> Maybe String
  -> Maybe String
  -> String
  -> AppContextM Int
createCountGroupByCoordinateFn entityName entityId mQuery enabledCondition mOrganizationId mEntityId mState stateCondition = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        f'
          "SELECT count(*) \
          \FROM %s \
          \LEFT JOIN registry_%s ON %s.organization_id = registry_%s.organization_id AND %s.%s = registry_%s.%s \
          \WHERE %s tenant_uuid = ? AND (name ~* ? OR id ~* ?) %s %s \
          \  AND id IN (SELECT CONCAT(organization_id, ':', %s, ':', (max(string_to_array(version, '.')::int[]))[1] || '.' || \
          \                                                             (max(string_to_array(version, '.')::int[]))[2] || '.' || \
          \                                                             (max(string_to_array(version, '.')::int[]))[3]) \
          \             FROM %s \
          \             WHERE tenant_uuid = ? \
          \               AND (name ~* ? OR id ~* ?) \
          \             GROUP BY organization_id, %s)"
          [ entityName
          , entityName
          , entityName
          , entityName
          , entityName
          , entityId
          , entityName
          , entityId
          , enabledCondition
          , mapToDBCoordinatesSql entityName entityId mOrganizationId mEntityId
          , stateCondition
          , entityId
          , entityName
          , entityId
          ]
  logInfo _CMP_DATABASE sql
  let action conn =
        query
          conn
          (fromString sql)
          ( [U.toString tenantUuid, regex mQuery, regex mQuery]
              ++ mapToDBCoordinatesParams mOrganizationId mEntityId
              ++ maybeToList mState
              ++ [U.toString tenantUuid]
              ++ [regex mQuery, regex mQuery]
          )
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0
