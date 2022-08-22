module Wizard.Database.DAO.Common
  ( module Shared.Database.DAO.Common
  , runInTransaction
  , createFindEntitiesGroupByCoordinatePageableQuerySortFn
  ) where

import Control.Monad.Reader (asks)
import Data.String (fromString)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple

import qualified Shared.Database.DAO.Common as S
import Shared.Database.DAO.Common hiding (runInTransaction)
import Shared.Database.Mapping.Package.Package ()
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

runInTransaction :: AppContextM a -> AppContextM a
runInTransaction = S.runInTransaction logInfoU logWarnU

createFindEntitiesGroupByCoordinatePageableQuerySortFn entityName pageLabel pageable sort fields entityId mQuery mEnabled mOrganizationId mEntityId
  -- 1. Prepare variables
 = do
  appUuid <- asks _appContextAppUuid
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
           \WHERE app_uuid = ? AND id IN ( \
           \    SELECT CONCAT(organization_id, ':', %s, ':', (max(string_to_array(version, '.')::int[]))[1] || '.' || \
           \                                                    (max(string_to_array(version, '.')::int[]))[2] || '.' || \
           \                                                    (max(string_to_array(version, '.')::int[]))[3]) \
           \    FROM %s \
           \    WHERE %s app_uuid = ? AND (name ~* ? OR id ~* ?) %s \
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
          , mapToDBCoordinatesSql entityId mOrganizationId mEntityId
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
          (U.toString appUuid :
           U.toString appUuid : regex mQuery : regex mQuery : mapToDBCoordinatesParams mOrganizationId mEntityId)
  entities <- runDB action
  -- 4. Constructor response
  let metadata =
        PageMetadata
          { _pageMetadataSize = sizeI
          , _pageMetadataTotalElements = count
          , _pageMetadataTotalPages = computeTotalPage count sizeI
          , _pageMetadataNumber = pageI
          }
  return $ Page pageLabel metadata entities

createCountGroupByCoordinateFn ::
     String -> String -> Maybe String -> String -> Maybe String -> Maybe String -> AppContextM Int
createCountGroupByCoordinateFn entityName entityId mQuery enabledCondition mOrganizationId mEntityId = do
  appUuid <- asks _appContextAppUuid
  let sql =
        f'
          "SELECT COUNT(*) \
          \ FROM (SELECT COUNT(*) \
          \   FROM %s \
          \   WHERE %s app_uuid = ? AND name ~* ? %s \
          \   GROUP BY organization_id, %s) p"
          [entityName, enabledCondition, mapToDBCoordinatesSql entityId mOrganizationId mEntityId, entityId]
  logInfo _CMP_DATABASE sql
  let action conn =
        query
          conn
          (fromString sql)
          (U.toString appUuid : regex mQuery : mapToDBCoordinatesParams mOrganizationId mEntityId)
  result <- runDB action
  case result of
    [count] -> return . fromOnly $ count
    _ -> return 0
