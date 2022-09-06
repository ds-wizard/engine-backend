module Wizard.Database.DAO.Branch.BranchDataDAO where

import Control.Lens ((^.))
import Control.Monad.Reader (asks)
import Data.String
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import LensesConfig
import Shared.Model.Event.Event
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Branch.BranchData ()
import Wizard.Database.Mapping.Branch.BranchDataLength ()
import Wizard.Model.Branch.BranchData
import Wizard.Model.Branch.BranchDataLength
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

entityName = "branch_data"

findBranchesForSquashing :: AppContextM [U.UUID]
findBranchesForSquashing = do
  let sql = "SELECT branch_uuid FROM branch_data"
  logInfoU _CMP_DATABASE sql
  let action conn = query_ conn (fromString sql)
  entities <- runDB action
  return . concat $ entities

findBranchDataById :: String -> AppContextM BranchData
findBranchDataById branchUuid = do
  appUuid <- asks _appContextAppUuid
  createFindEntityWithFieldsByFn "*" False entityName [appQueryUuid appUuid, ("branch_uuid", branchUuid)]

findBranchDataByIdForSquashingLocked :: String -> AppContextM BranchData
findBranchDataByIdForSquashingLocked branchUuid =
  createFindEntityWithFieldsByFn "*" True entityName [("branch_uuid", branchUuid)]

findBranchDataLengthById :: String -> AppContextM BranchDataLength
findBranchDataLengthById branchUuid =
  createFindEntityWithFieldsByFn "branch_uuid, json_array_length(events)" True entityName [("branch_uuid", branchUuid)]

insertBranchData :: BranchData -> AppContextM Int64
insertBranchData = createInsertFn entityName

updateBranchDataById :: BranchData -> AppContextM Int64
updateBranchDataById branchData = do
  appUuid <- asks _appContextAppUuid
  let sql =
        fromString
          "UPDATE branch_data SET branch_uuid = ?, metamodel_version = ?, events = ?, app_uuid = ?, created_at = ?, updated_at = ? WHERE app_uuid = ? AND branch_uuid = ?"
  let params = toRow branchData ++ [toField appUuid, toField . U.toText $ branchData ^. branchUuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action

appendBranchEventByUuid :: String -> [Event] -> AppContextM ()
appendBranchEventByUuid qtnUuid events = do
  appUuid <- asks _appContextAppUuid
  let sql =
        fromString "UPDATE branch_data SET events = events::jsonb || ?::jsonb WHERE app_uuid = ? AND branch_uuid = ?"
  let params = [toJSONField events, toField appUuid, toField qtnUuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()

updateBranchEventsByUuid :: String -> [Event] -> AppContextM ()
updateBranchEventsByUuid branchUuid events = do
  let sql = fromString "UPDATE branch_data SET events = ? WHERE branch_uuid = ?"
  let params = [toJSONField events, toField branchUuid]
  logInsertAndUpdate sql params
  let action conn = execute conn sql params
  runDB action
  return ()

deleteBranchDatas :: AppContextM Int64
deleteBranchDatas = createDeleteEntitiesFn entityName

deleteBranchDataById :: String -> AppContextM Int64
deleteBranchDataById branchUuid = createDeleteEntityByFn entityName [("branch_uuid", branchUuid)]
