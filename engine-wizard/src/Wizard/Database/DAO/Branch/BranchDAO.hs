module Wizard.Database.DAO.Branch.BranchDAO where

import Control.Lens ((^.))
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import LensesConfig
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Branch.Branch ()
import Wizard.Database.Mapping.Branch.BranchWithEvents ()
import Wizard.Model.Branch.Branch
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

entityName = "branch"

pageLabel = "branches"

findBranches :: AppContextM [Branch]
findBranches = createFindEntitiesFn entityName

findBranchesByPreviousPackageId :: String -> AppContextM [Branch]
findBranchesByPreviousPackageId previousPackageId =
  createFindEntitiesByFn entityName [("previous_package_id", previousPackageId)]

findBranchesWithEvents :: AppContextM [BranchWithEvents]
findBranchesWithEvents = createFindEntitiesFn entityName

findBranchesWithEventsPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page BranchWithEvents)
findBranchesWithEventsPage mQuery pageable sort =
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "*"
    "name ~* ? OR km_id ~* ?"
    [regex mQuery, regex mQuery]

findBranchById :: String -> AppContextM Branch
findBranchById = createFindEntityByFn entityName "uuid"

findBranchByKmId :: String -> AppContextM Branch
findBranchByKmId = createFindEntityByFn entityName "km_id"

findBranchByKmId' :: String -> AppContextM (Maybe Branch)
findBranchByKmId' = createFindEntityByFn' entityName "km_id"

findBranchWithEventsById :: String -> AppContextM BranchWithEvents
findBranchWithEventsById = createFindEntityByFn entityName "uuid"

insertBranch :: BranchWithEvents -> AppContextM Int64
insertBranch = createInsertFn entityName

updateBranchById :: BranchWithEvents -> AppContextM Int64
updateBranchById branch = do
  let params = toRow branch ++ [toField . U.toText $ branch ^. uuid]
  let sql =
        "UPDATE branch SET uuid = ?, name = ?, km_id = ?, metamodel_version = ?, previous_package_id = ?, events = ?, owner_uuid = ?, created_at = ?, updated_at = ? WHERE uuid = ?"
  logInfoU _CMP_DATABASE sql
  let action conn = execute conn (fromString sql) params
  runDB action

updateBranchWithMigrationInfo :: String -> String -> String -> AppContextM Int64
updateBranchWithMigrationInfo branchUuid forkOfPackageId mergeCheckpointPackageId = do
  let params = [toField forkOfPackageId, toField mergeCheckpointPackageId, toField branchUuid]
  let sql = "UPDATE branch SET forkOfPackageId = ?, mergeCheckpointPackageId = ? WHERE uuid = ?"
  logInfoU _CMP_DATABASE sql
  let action conn = execute conn (fromString sql) params
  runDB action

updateBranchWithPreviousPackageId :: String -> String -> AppContextM Int64
updateBranchWithPreviousPackageId branchUuid previousPackageId = do
  let params = [toField previousPackageId, toField branchUuid]
  let sql = "UPDATE branch SET previousPackageId = ? WHERE uuid = ?"
  logInfoU _CMP_DATABASE sql
  let action conn = execute conn (fromString sql) params
  runDB action

deleteBranches :: AppContextM Int64
deleteBranches = createDeleteEntitiesFn entityName

deleteBranchById :: String -> AppContextM Int64
deleteBranchById = createDeleteEntityByFn entityName "uuid"
