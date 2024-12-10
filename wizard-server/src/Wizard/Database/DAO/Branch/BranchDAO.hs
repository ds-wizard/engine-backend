module Wizard.Database.DAO.Branch.BranchDAO where

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
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Branch.Branch ()
import Wizard.Database.Mapping.Branch.BranchList ()
import Wizard.Database.Mapping.Branch.BranchSuggestion ()
import Wizard.Model.Branch.Branch
import Wizard.Model.Branch.BranchList
import Wizard.Model.Branch.BranchSuggestion
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

entityName = "branch"

pageLabel = "branches"

findBranches :: AppContextM [Branch]
findBranches = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid]

findBranchesByPreviousPackageId :: String -> AppContextM [Branch]
findBranchesByPreviousPackageId previousPackageId = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesByFn entityName [tenantQueryUuid tenantUuid, ("previous_package_id", previousPackageId)]

findBranchesPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page BranchList)
findBranchesPage mQuery pageable sort =
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
              "SELECT branch.uuid, \
              \       branch.name, \
              \       branch.km_id, \
              \       branch.version, \
              \       get_branch_state(knowledge_model_migration, branch_data, get_branch_fork_of_package_id(tenant_config, previous_pkg, branch), '%s') as state, \
              \       branch.previous_package_id, \
              \       get_branch_fork_of_package_id(tenant_config, previous_pkg, branch) as fork_of_package_id, \
              \       branch.created_by, \
              \       branch.created_at, \
              \       branch_data.updated_at  \
              \FROM branch \
              \         JOIN branch_data ON branch.uuid = branch_data.branch_uuid \
              \         JOIN tenant_config ON branch.tenant_uuid = tenant_config.uuid \
              \         LEFT JOIN knowledge_model_migration ON branch.uuid = knowledge_model_migration.branch_uuid \
              \         LEFT JOIN package previous_pkg \
              \                   ON branch.previous_package_id = previous_pkg.id and branch.tenant_uuid = previous_pkg.tenant_uuid \
              \WHERE (branch.name ~* ? OR branch.km_id ~* ?) AND branch.tenant_uuid = ? \
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

findBranchSuggestionsPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page BranchSuggestion)
findBranchSuggestionsPage mQuery pageable sort = do
  tenantUuid <- asks currentTenantUuid
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "uuid, name"
    "WHERE name ~* ? AND tenant_uuid = ?"
    [regexM mQuery, U.toString tenantUuid]

findBranchByUuid :: U.UUID -> AppContextM Branch
findBranchByUuid uuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

findBranchByKmId :: String -> AppContextM Branch
findBranchByKmId kmId = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn entityName [tenantQueryUuid tenantUuid, ("km_id", kmId)]

findBranchByKmId' :: String -> AppContextM (Maybe Branch)
findBranchByKmId' kmId = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityByFn' entityName [tenantQueryUuid tenantUuid, ("km_id", kmId)]

findBranchSuggestionByUuid' :: U.UUID -> AppContextM (Maybe BranchSuggestion)
findBranchSuggestionByUuid' uuid = do
  tenantUuid <- asks currentTenantUuid
  createFindEntityWithFieldsByFn' "uuid, name" entityName [tenantQueryUuid tenantUuid, ("uuid", U.toString uuid)]

countBranches :: AppContextM Int
countBranches = do
  tenantUuid <- asks currentTenantUuid
  countBranchesWithTenant tenantUuid

countBranchesWithTenant :: U.UUID -> AppContextM Int
countBranchesWithTenant tenantUuid = createCountByFn entityName tenantCondition [U.toString tenantUuid]

insertBranch :: Branch -> AppContextM Int64
insertBranch = createInsertFn entityName

updateBranchById :: Branch -> AppContextM Int64
updateBranchById branch = do
  tenantUuid <- asks currentTenantUuid
  let sql =
        fromString
          "BEGIN TRANSACTION; \
          \UPDATE branch SET uuid = ?, name = ?, km_id = ?, previous_package_id = ?, created_by = ?, created_at = ?, updated_at = ?, tenant_uuid = ?, version = ?, description = ?, readme = ?, license = ? WHERE tenant_uuid = ? AND uuid = ?; \
          \UPDATE branch_data SET updated_at = now() WHERE tenant_uuid = ? AND branch_uuid = ?; \
          \COMMIT;"
  let params = toRow branch ++ [toField tenantUuid, toField . U.toText $ branch.uuid, toField tenantUuid, toField . U.toText $ branch.uuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

clearBranchCreatedBy :: U.UUID -> AppContextM ()
clearBranchCreatedBy userUuid = do
  let sql = fromString "UPDATE branch SET created_by = null WHERE created_by = ?"
  let params = [toField userUuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()

deleteBranches :: AppContextM Int64
deleteBranches = createDeleteEntitiesFn entityName

deleteBranchByUuid :: U.UUID -> AppContextM Int64
deleteBranchByUuid uuid = createDeleteEntityByFn entityName [("uuid", U.toString uuid)]
