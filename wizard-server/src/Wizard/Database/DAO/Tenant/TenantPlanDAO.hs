module Wizard.Database.DAO.Tenant.TenantPlanDAO where

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
import Wizard.Database.Mapping.Tenant.TenantPlan ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Tenant.Plan.TenantPlan

entityName = "tenant_plan"

findPlans :: AppContextM [TenantPlan]
findPlans = createFindEntitiesFn entityName

findPlansForTenantUuid :: U.UUID -> AppContextM [TenantPlan]
findPlansForTenantUuid tenantUuid =
  createFindEntitiesBySortedFn entityName [tenantQueryUuid tenantUuid] [Sort "since" Descending]

findPlanByUuid :: U.UUID -> AppContextM TenantPlan
findPlanByUuid uuid = createFindEntityByFn entityName [("uuid", U.toString uuid)]

insertPlan :: TenantPlan -> AppContextM Int64
insertPlan = createInsertFn entityName

updatePlanByUuid :: TenantPlan -> AppContextM TenantPlan
updatePlanByUuid tenant = do
  now <- liftIO getCurrentTime
  let updatedTenant = tenant {updatedAt = now}
  let sql =
        fromString
          "UPDATE tenant_plan SET uuid = ?, name = ?, users = ?, since = ?, until = ?, test = ?, tenant_uuid = ?, created_at = ?, updated_at = ? WHERE uuid = ?"
  let params = toRow updatedTenant ++ [toField updatedTenant.uuid]
  logQuery sql params
  let action conn = execute conn sql params
  runDB action
  return updatedTenant

deletePlans :: AppContextM Int64
deletePlans = createDeleteEntitiesFn entityName

deletePlanByUuid :: U.UUID -> AppContextM Int64
deletePlanByUuid uuid = createDeleteEntityByFn entityName [("uuid", U.toString uuid)]
