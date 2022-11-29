module Wizard.Database.DAO.Plan.AppPlanDAO where

import Control.Monad.Reader (liftIO)
import Data.String
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Model.Common.Sort
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Plan.AppPlan ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Plan.AppPlan

entityName = "app_plan"

findAppPlans :: AppContextM [AppPlan]
findAppPlans = createFindEntitiesFn entityName

findAppPlansForAppUuid :: String -> AppContextM [AppPlan]
findAppPlansForAppUuid appUuid =
  createFindEntitiesBySortedFn entityName [appQueryString appUuid] [Sort "since" Descending]

findAppPlanById :: String -> AppContextM AppPlan
findAppPlanById uuid = createFindEntityByFn entityName [("uuid", uuid)]

insertAppPlan :: AppPlan -> AppContextM Int64
insertAppPlan = createInsertFn entityName

updateAppPlanById :: AppPlan -> AppContextM AppPlan
updateAppPlanById app = do
  now <- liftIO getCurrentTime
  let updatedApp = app {updatedAt = now}
  let sql =
        fromString
          "UPDATE app_plan SET uuid = ?, name = ?, users = ?, since = ?, until = ?, test = ?, app_uuid = ?, created_at = ?, updated_at = ? WHERE uuid = ?"
  let params = toRow updatedApp ++ [toField updatedApp.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return updatedApp

deleteAppPlans :: AppContextM Int64
deleteAppPlans = createDeleteEntitiesFn entityName

deleteAppPlanById :: String -> AppContextM Int64
deleteAppPlanById uuid = createDeleteEntityByFn entityName [("uuid", uuid)]
