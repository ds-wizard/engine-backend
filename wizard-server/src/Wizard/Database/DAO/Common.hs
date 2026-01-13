module Wizard.Database.DAO.Common (
  module Shared.Common.Database.DAO.Common,
  runInTransaction,
  createFindEntitiesGroupByCoordinatePageableQuerySortFn,
  createFindEntitiesGroupByCoordinatePageableQuerySortFn',
  createCountGroupByCoordinateFn,
  createCountGroupByCoordinateFn',
) where

import Control.Monad.Reader (asks)
import Data.Maybe (maybeToList)
import Data.String (fromString)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField

import Shared.Common.Database.DAO.Common hiding (runInTransaction)
import qualified Shared.Common.Database.DAO.Common as S
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Util.Logger
import Shared.Common.Util.String
import Shared.KnowledgeModel.Database.Mapping.KnowledgeModel.Package.KnowledgeModelPackage ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

runInTransaction :: AppContextM a -> AppContextM a
runInTransaction = S.runInTransaction logInfoI logWarnI

createFindEntitiesGroupByCoordinatePageableQuerySortFn entityName pageLabel pageable sort fields entityId mQuery mEnabled mOrganizationId mEntityId mOutdated outdatedCondition =
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
        mOutdated
        outdatedCondition
    -- 3. Get entities
    let sql =
          f''
            "SELECT ${fields} \
            \FROM ${entityName} \
            \LEFT JOIN registry_${entityName} ON ${entityName}.organization_id = registry_${entityName}.organization_id AND ${entityName}.${entityId} = registry_${entityName}.${entityId} \
            \LEFT JOIN registry_organization ON ${entityName}.organization_id = registry_organization.organization_id \
            \WHERE tenant_uuid = ? AND id IN ( \
            \    SELECT CONCAT(organization_id, ':', ${entityId}, ':', (max(string_to_array(version, '.')::int[]))[1] || '.' || \
            \                                                          (max(string_to_array(version, '.')::int[]))[2] || '.' || \
            \                                                          (max(string_to_array(version, '.')::int[]))[3]) \
            \    FROM ${entityName} \
            \    WHERE ${enabledCondition} tenant_uuid = ? AND (name ~* ? OR id ~* ?) ${coordinateSql} \
            \    GROUP BY organization_id, ${entityId} \
            \) \
            \${outdatedCondition} \
            \${sort} \
            \OFFSET ${offset} \
            \LIMIT ${limit}"
            [ ("fields", fields)
            , ("entityName", entityName)
            , ("entityId", entityId)
            , ("enabledCondition", enabledCondition)
            , ("coordinateSql", mapToDBCoordinatesSql entityName entityId mOrganizationId mEntityId)
            , ("outdatedCondition", outdatedCondition)
            , ("sort", mapSort sort)
            , ("offset", show skip)
            , ("limit", show sizeI)
            ]
    logInfoI _CMP_DATABASE (trim sql)
    let action conn =
          query
            conn
            (fromString sql)
            ( toField tenantUuid
                : toField tenantUuid
                : toField (regexM mQuery)
                : toField (regexM mQuery)
                : fmap toField (mapToDBCoordinatesParams mOrganizationId mEntityId)
                ++ (maybeToList . fmap toField $ mOutdated)
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
  -> Maybe Bool
  -> String
  -> AppContextM Int
createCountGroupByCoordinateFn entityName entityId mQuery enabledCondition mOrganizationId mEntityId mOutdated outdatedCondition = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        f''
          "SELECT count(*) \
          \FROM ${entityName} \
          \LEFT JOIN registry_${entityName} ON ${entityName}.organization_id = registry_${entityName}.organization_id AND ${entityName}.${entityId} = registry_${entityName}.${entityId} \
          \WHERE ${enabledCondition} tenant_uuid = ? AND (name ~* ? OR id ~* ?) ${coordinateSql} ${outdatedCondition} \
          \  AND id IN (SELECT CONCAT(organization_id, ':', ${entityId}, ':', (max(string_to_array(version, '.')::int[]))[1] || '.' || \
          \                                                                   (max(string_to_array(version, '.')::int[]))[2] || '.' || \
          \                                                                   (max(string_to_array(version, '.')::int[]))[3]) \
          \             FROM ${entityName} \
          \             WHERE tenant_uuid = ? \
          \               AND (name ~* ? OR id ~* ?) \
          \             GROUP BY organization_id, ${entityId})"
          [ ("entityName", entityName)
          , ("entityId", entityId)
          , ("enabledCondition", enabledCondition)
          , ("coordinateSql", mapToDBCoordinatesSql entityName entityId mOrganizationId mEntityId)
          , ("outdatedCondition", outdatedCondition)
          ]
  logInfo _CMP_DATABASE sql
  let action conn =
        query
          conn
          (fromString sql)
          ( [toField tenantUuid, toField (regexM mQuery), toField (regexM mQuery)]
              ++ fmap toField (mapToDBCoordinatesParams mOrganizationId mEntityId)
              ++ maybeToList (fmap toField mOutdated)
              ++ [toField tenantUuid]
              ++ [toField $ regexM mQuery, toField $ regexM mQuery]
          )
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0

createFindEntitiesGroupByCoordinatePageableQuerySortFn' entityName pageLabel pageable sort fields entityId mQuery mEnabled mOrganizationId mEntityId mOutdated outdatedCondition =
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
      createCountGroupByCoordinateFn'
        entityName
        entityId
        mQuery
        enabledCondition
        mOrganizationId
        mEntityId
        mOutdated
        outdatedCondition
    -- 3. Get entities
    let sql =
          f''
            "SELECT ${fields} \
            \FROM ${entityName} \
            \LEFT JOIN registry_${entityName} ON ${entityName}.organization_id = registry_${entityName}.organization_id AND ${entityName}.${entityId} = registry_${entityName}.${entityId} \
            \LEFT JOIN registry_organization ON ${entityName}.organization_id = registry_organization.organization_id \
            \WHERE tenant_uuid = ? AND concat(${entityName}.organization_id, ':', ${entityName}.${entityId}, ':', ${entityName}.version) IN ( \
            \    SELECT CONCAT(organization_id, ':', ${entityId}, ':', (max(string_to_array(version, '.')::int[]))[1] || '.' || \
            \                                                          (max(string_to_array(version, '.')::int[]))[2] || '.' || \
            \                                                          (max(string_to_array(version, '.')::int[]))[3]) \
            \    FROM ${entityName} \
            \    WHERE ${enabledCondition} tenant_uuid = ? AND (name ~* ? OR organization_id ~* ? OR ${entityId} ~* ? OR version ~* ?) ${coordinateSql} \
            \    GROUP BY organization_id, ${entityId} \
            \) \
            \${outdatedCondition} \
            \${sort} \
            \OFFSET ${offset} \
            \LIMIT ${limit}"
            [ ("fields", fields)
            , ("entityName", entityName)
            , ("entityId", entityId)
            , ("enabledCondition", enabledCondition)
            , ("coordinateSql", mapToDBCoordinatesSql entityName entityId mOrganizationId mEntityId)
            , ("outdatedCondition", outdatedCondition)
            , ("sort", mapSort sort)
            , ("offset", show skip)
            , ("limit", show sizeI)
            ]
    logInfoI _CMP_DATABASE (trim sql)
    let action conn =
          query
            conn
            (fromString sql)
            ( toField tenantUuid
                : toField tenantUuid
                : toField (regexM mQuery)
                : toField (regexM mQuery)
                : toField (regexM mQuery)
                : toField (regexM mQuery)
                : fmap toField (mapToDBCoordinatesParams mOrganizationId mEntityId)
                ++ (maybeToList . fmap toField $ mOutdated)
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

createCountGroupByCoordinateFn'
  :: String
  -> String
  -> Maybe String
  -> String
  -> Maybe String
  -> Maybe String
  -> Maybe Bool
  -> String
  -> AppContextM Int
createCountGroupByCoordinateFn' entityName entityId mQuery enabledCondition mOrganizationId mEntityId mOutdated outdatedCondition = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        f''
          "SELECT count(*) \
          \FROM ${entityName} \
          \LEFT JOIN registry_${entityName} ON ${entityName}.organization_id = registry_${entityName}.organization_id AND ${entityName}.${entityId} = registry_${entityName}.${entityId} \
          \WHERE ${enabledCondition} tenant_uuid = ? AND (name ~* ? OR ${entityName}.organization_id ~* ? OR ${entityName}.${entityId} ~* ? OR ${entityName}.version ~* ?) ${coordinateSql} ${outdatedCondition} \
          \  AND concat(${entityName}.organization_id, ':', ${entityName}.${entityId}, ':', ${entityName}.version) IN \
          \      (SELECT CONCAT(organization_id, ':', locale_id, ':', \
          \                     (max(string_to_array(version, '.')::int[]))[1] || '.' || \
          \                     (max(string_to_array(version, '.')::int[]))[2] || '.' || \
          \                     (max(string_to_array(version, '.')::int[]))[3]) \
          \             FROM ${entityName} \
          \             WHERE tenant_uuid = ? \
          \               AND (name ~* ? OR ${entityName}.organization_id ~* ? OR ${entityName}.${entityId} ~* ? OR ${entityName}.version ~* ?) \
          \             GROUP BY ${entityName}.organization_id, ${entityName}.${entityId})"
          [ ("entityName", entityName)
          , ("entityId", entityId)
          , ("enabledCondition", enabledCondition)
          , ("coordinateSql", mapToDBCoordinatesSql entityName entityId mOrganizationId mEntityId)
          , ("outdatedCondition", outdatedCondition)
          ]
  logInfo _CMP_DATABASE sql
  let action conn =
        query
          conn
          (fromString sql)
          ( [toField tenantUuid, toField (regexM mQuery), toField (regexM mQuery), toField (regexM mQuery), toField (regexM mQuery)]
              ++ fmap toField (mapToDBCoordinatesParams mOrganizationId mEntityId)
              ++ maybeToList (fmap toField mOutdated)
              ++ [toField tenantUuid]
              ++ [toField $ regexM mQuery, toField $ regexM mQuery, toField (regexM mQuery), toField (regexM mQuery)]
          )
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0
