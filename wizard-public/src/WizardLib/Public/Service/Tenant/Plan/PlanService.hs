module WizardLib.Public.Service.Tenant.Plan.PlanService where

import Control.Monad.Reader (ask, liftIO)
import Data.Time
import qualified Data.UUID as U
import Prelude hiding (until)

import Shared.Common.Constant.Acl
import Shared.Common.Model.Context.AppContext
import Shared.Common.Service.Acl.AclService
import Shared.Common.Util.Uuid
import WizardLib.Public.Api.Resource.Tenant.Plan.TenantPlanChangeDTO
import WizardLib.Public.Database.DAO.Tenant.TenantPlanDAO
import WizardLib.Public.Model.Tenant.Plan.TenantPlan
import WizardLib.Public.Service.Tenant.Plan.PlanMapper

getPlansForCurrentTenant :: AppContextC s sc m => m [TenantPlan]
getPlansForCurrentTenant = do
  context <- ask
  findTenantPlansForTenantUuid context.tenantUuid'

createPlan' :: AppContextC s sc m => (U.UUID -> m tenant) -> (tenant -> m ()) -> U.UUID -> TenantPlanChangeDTO -> m TenantPlan
createPlan' findTenantByUuid recomputePlansForTenant tenantUuid reqDto = do
  uuid <- liftIO generateUuid
  createPlanWithUuid' findTenantByUuid recomputePlansForTenant uuid tenantUuid reqDto

createPlanWithUuid' :: AppContextC s sc m => (U.UUID -> m tenant) -> (tenant -> m ()) -> U.UUID -> U.UUID -> TenantPlanChangeDTO -> m TenantPlan
createPlanWithUuid' findTenantByUuid recomputePlansForTenant uuid tenantUuid reqDto = do
  checkPermission _TENANT_PERM
  tenant <- findTenantByUuid tenantUuid
  now <- liftIO getCurrentTime
  let plan = fromChangeDTO reqDto uuid tenantUuid now now
  insertTenantPlan plan
  recomputePlansForTenant tenant
  return plan

modifyPlan' :: AppContextC s sc m => (U.UUID -> m tenant) -> (tenant -> m ()) -> U.UUID -> U.UUID -> TenantPlanChangeDTO -> m TenantPlan
modifyPlan' findTenantByUuid recomputePlansForTenant aUuid pUuid reqDto = do
  checkPermission _TENANT_PERM
  tenant <- findTenantByUuid aUuid
  plan <- findTenantPlanByUuid pUuid
  let updatedPlan = fromChangeDTO reqDto plan.uuid plan.tenantUuid plan.createdAt plan.updatedAt
  updateTenantPlanByUuid updatedPlan
  recomputePlansForTenant tenant
  return updatedPlan

deletePlan' :: AppContextC s sc m => (U.UUID -> m tenant) -> (tenant -> m ()) -> U.UUID -> U.UUID -> m ()
deletePlan' findTenantByUuid recomputePlansForTenant aUuid pUuid = do
  checkPermission _TENANT_PERM
  tenant <- findTenantByUuid aUuid
  plan <- findTenantPlanByUuid pUuid
  deleteTenantPlanByUuid pUuid
  recomputePlansForTenant tenant
  return ()
