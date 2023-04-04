module Wizard.Database.DAO.Branch.BranchDAO where

import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Branch.Branch ()
import Wizard.Database.Mapping.Branch.BranchList ()
import Wizard.Model.Branch.Branch
import Wizard.Model.Branch.BranchList
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

entityName = "branch"

pageLabel = "branches"

findBranches :: AppContextM [Branch]
findBranches = do
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findBranchesByPreviousPackageId :: String -> AppContextM [Branch]
findBranchesByPreviousPackageId previousPackageId = do
  appUuid <- asks currentAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("previous_package_id", previousPackageId)]

findBranchesPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page BranchList)
findBranchesPage mQuery pageable sort =
  -- 1. Prepare variables
  do
    appUuid <- asks currentAppUuid
    let condition = "WHERE (name ~* ? OR km_id ~* ?) AND app_uuid = ?"
    let conditionParams = [regex mQuery, regex mQuery, U.toString appUuid]
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
              \       get_branch_state(knowledge_model_migration, branch_data, get_branch_fork_of_package_id(app_config, previous_pkg, branch), '%s') as state, \
              \       branch.previous_package_id, \
              \       get_branch_fork_of_package_id(app_config, previous_pkg, branch) as fork_of_package_id, \
              \       branch.created_by, \
              \       branch.created_at, \
              \       branch.updated_at  \
              \FROM branch \
              \         JOIN branch_data ON branch.uuid = branch_data.branch_uuid \
              \         JOIN app_config ON branch.app_uuid = app_config.uuid \
              \         LEFT JOIN knowledge_model_migration ON branch.uuid = knowledge_model_migration.branch_uuid \
              \         LEFT JOIN package previous_pkg \
              \                   ON branch.previous_package_id = previous_pkg.id and branch.app_uuid = previous_pkg.app_uuid \
              \WHERE (branch.name ~* ? OR branch.km_id ~* ?) AND branch.app_uuid = ? \
              \%s OFFSET %s LIMIT %s"
              [U.toString appUuid, mapSort sort, show skip, show sizeI]
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

findBranchByUuid :: U.UUID -> AppContextM Branch
findBranchByUuid uuid = do
  appUuid <- asks currentAppUuid
  createFindEntityByFn entityName [appQueryUuid appUuid, ("uuid", U.toString uuid)]

findBranchByKmId :: String -> AppContextM Branch
findBranchByKmId kmId = do
  appUuid <- asks currentAppUuid
  createFindEntityByFn entityName [appQueryUuid appUuid, ("km_id", kmId)]

findBranchByKmId' :: String -> AppContextM (Maybe Branch)
findBranchByKmId' kmId = do
  appUuid <- asks currentAppUuid
  createFindEntityByFn' entityName [appQueryUuid appUuid, ("km_id", kmId)]

countBranches :: AppContextM Int
countBranches = do
  appUuid <- asks currentAppUuid
  countBranchesWithApp appUuid

countBranchesWithApp :: U.UUID -> AppContextM Int
countBranchesWithApp appUuid = createCountByFn entityName appCondition [U.toString appUuid]

insertBranch :: Branch -> AppContextM Int64
insertBranch = createInsertFn entityName

updateBranchById :: Branch -> AppContextM Int64
updateBranchById branch = do
  appUuid <- asks currentAppUuid
  let sql =
        fromString
          "UPDATE branch SET uuid = ?, name = ?, km_id = ?, previous_package_id = ?, created_by = ?, created_at = ?, updated_at = ?, app_uuid = ?, version = ?, description = ?, readme = ?, license = ? WHERE app_uuid = ? AND uuid = ?"
  let params = toRow branch ++ [toField appUuid, toField . U.toText $ branch.uuid]
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
