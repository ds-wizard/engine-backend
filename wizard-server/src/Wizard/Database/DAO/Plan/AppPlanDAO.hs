module Wizard.Database.DAO.Plan.AppPlanDAO where

import Control.Monad.Reader (liftIO)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Model.Common.Sort
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Plan.AppPlan ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Plan.AppPlan

entityName = "app_plan"

findAppPlans :: AppContextM [AppPlan]
findAppPlans = createFindEntitiesFn entityName

findAppPlansForAppUuid :: U.UUID -> AppContextM [AppPlan]
findAppPlansForAppUuid appUuid =
  createFindEntitiesBySortedFn entityName [appQueryUuid appUuid] [Sort "since" Descending]

findAppPlanByUuid :: U.UUID -> AppContextM AppPlan
findAppPlanByUuid uuid = createFindEntityByFn entityName [("uuid", U.toString uuid)]

insertAppPlan :: AppPlan -> AppContextM Int64
insertAppPlan = createInsertFn entityName

updateAppPlanByUuid :: AppPlan -> AppContextM AppPlan
updateAppPlanByUuid app = do
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

deleteAppPlanByUuid :: U.UUID -> AppContextM Int64
deleteAppPlanByUuid uuid = createDeleteEntityByFn entityName [("uuid", U.toString uuid)]
