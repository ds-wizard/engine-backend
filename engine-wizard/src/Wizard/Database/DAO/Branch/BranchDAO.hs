module Wizard.Database.DAO.Branch.BranchDAO where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
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
findBranches = do
  appUuid <- asks _appContextAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid]

findBranchesByPreviousPackageId :: String -> AppContextM [Branch]
findBranchesByPreviousPackageId previousPackageId = do
  appUuid <- asks _appContextAppUuid
  createFindEntitiesByFn entityName [appQueryUuid appUuid, ("previous_package_id", previousPackageId)]

findBranchesWithEventsPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page BranchWithEvents)
findBranchesWithEventsPage mQuery pageable sort = do
  appUuid <- asks _appContextAppUuid
  createFindEntitiesPageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "*"
    "(name ~* ? OR km_id ~* ?) AND app_uuid = ?"
    [regex mQuery, regex mQuery, U.toString appUuid]

findBranchById :: String -> AppContextM Branch
findBranchById uuid = do
  appUuid <- asks _appContextAppUuid
  createFindEntityByFn entityName [appQueryUuid appUuid, ("uuid", uuid)]

findBranchByKmId :: String -> AppContextM Branch
findBranchByKmId kmId = do
  appUuid <- asks _appContextAppUuid
  createFindEntityByFn entityName [appQueryUuid appUuid, ("km_id", kmId)]

findBranchByKmId' :: String -> AppContextM (Maybe Branch)
findBranchByKmId' kmId = do
  appUuid <- asks _appContextAppUuid
  createFindEntityByFn' entityName [appQueryUuid appUuid, ("km_id", kmId)]

findBranchWithEventsById :: String -> AppContextM BranchWithEvents
findBranchWithEventsById uuid = do
  appUuid <- asks _appContextAppUuid
  createFindEntityByFn entityName [appQueryUuid appUuid, ("uuid", uuid)]

insertBranch :: BranchWithEvents -> AppContextM Int64
insertBranch = createInsertFn entityName

updateBranchById :: BranchWithEvents -> AppContextM Int64
updateBranchById branch = do
  appUuid <- asks _appContextAppUuid
  let params = toRow branch ++ [toField appUuid, toField . U.toText $ branch ^. uuid]
  let sql =
        "UPDATE branch SET uuid = ?, name = ?, km_id = ?, metamodel_version = ?, previous_package_id = ?, events = ?, owner_uuid = ?, created_at = ?, updated_at = ?, app_uuid = ? WHERE app_uuid = ? AND uuid = ?"
  logInfoU _CMP_DATABASE sql
  let action conn = execute conn (fromString sql) params
  runDB action

deleteBranches :: AppContextM Int64
deleteBranches = createDeleteEntitiesFn entityName

deleteBranchById :: String -> AppContextM Int64
deleteBranchById uuid = createDeleteEntityByFn entityName [("uuid", uuid)]
