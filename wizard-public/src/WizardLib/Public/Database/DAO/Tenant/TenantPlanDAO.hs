module WizardLib.Public.Database.DAO.Tenant.TenantPlanDAO where

import Control.Monad.Reader (liftIO)
import Data.String
import Data.Time
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Int

import Shared.Common.Database.DAO.Common
import Shared.Common.Model.Common.Sort
import Shared.Common.Model.Context.AppContext
import WizardLib.Public.Database.Mapping.Tenant.TenantPlan ()
import WizardLib.Public.Model.Tenant.Plan.TenantPlan

entityName = "tenant_plan"

findTenantPlans :: AppContextC s sc m => m [TenantPlan]
findTenantPlans = createFindEntitiesFn entityName

findTenantPlansForTenantUuid :: AppContextC s sc m => U.UUID -> m [TenantPlan]
findTenantPlansForTenantUuid tenantUuid =
  createFindEntitiesBySortedFn entityName [tenantQueryUuid tenantUuid] [Sort "since" Descending]

findTenantPlanByUuid :: AppContextC s sc m => U.UUID -> m TenantPlan
findTenantPlanByUuid uuid = createFindEntityByFn entityName [("uuid", U.toString uuid)]

insertTenantPlan :: AppContextC s sc m => TenantPlan -> m Int64
insertTenantPlan = createInsertFn entityName

updateTenantPlanByUuid :: AppContextC s sc m => TenantPlan -> m TenantPlan
updateTenantPlanByUuid tenant = do
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

deleteTenantPlans :: AppContextC s sc m => m Int64
deleteTenantPlans = createDeleteEntitiesFn entityName

deleteTenantPlansByTenantUuid :: AppContextC s sc m => U.UUID -> m Int64
deleteTenantPlansByTenantUuid tenantUuid = createDeleteEntityByFn entityName [("tenant_uuid", U.toString tenantUuid)]

deleteTenantPlanByUuid :: AppContextC s sc m => U.UUID -> m Int64
deleteTenantPlanByUuid uuid = createDeleteEntityByFn entityName [("uuid", U.toString uuid)]
