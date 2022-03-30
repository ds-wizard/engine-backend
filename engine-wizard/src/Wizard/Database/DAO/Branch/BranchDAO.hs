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
import Wizard.Model.Branch.Branch
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

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

findBranchesPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page Branch)
findBranchesPage mQuery pageable sort = do
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

countBranches :: AppContextM Int
countBranches = do
  appUuid <- asks _appContextAppUuid
  countBranchesWithApp (U.toString appUuid)

countBranchesWithApp :: String -> AppContextM Int
countBranchesWithApp appUuid = createCountByFn entityName appCondition [appUuid]

insertBranch :: Branch -> AppContextM Int64
insertBranch = createInsertFn entityName

updateBranchById :: Branch -> AppContextM Int64
updateBranchById branch = do
  appUuid <- asks _appContextAppUuid
  let sql =
        fromString
          "UPDATE branch SET uuid = ?, name = ?, km_id = ?, previous_package_id = ?, owner_uuid = ?, created_at = ?, updated_at = ?, app_uuid = ? WHERE app_uuid = ? AND uuid = ?"
  let params = toRow branch ++ [toField appUuid, toField . U.toText $ branch ^. uuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

deleteBranches :: AppContextM Int64
deleteBranches = createDeleteEntitiesFn entityName

deleteBranchById :: String -> AppContextM Int64
deleteBranchById uuid = createDeleteEntityByFn entityName [("uuid", uuid)]
