module Wizard.Service.Tenant.Plan.PlanService where

import Control.Monad (void, when)
import Control.Monad.Reader (asks, liftIO)
import Data.Foldable (traverse_)
import Data.Maybe (isJust)
import Data.Time
import qualified Data.UUID as U
import Prelude hiding (until)

import Shared.Common.Util.List
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Tenant.Plan.TenantPlanChangeDTO
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Database.DAO.Tenant.TenantPlanDAO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Plan.TenantPlan
import Wizard.Model.Tenant.Tenant
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.Limit.LimitService
import Wizard.Service.Tenant.Plan.PlanMapper

getPlansForCurrentTenant :: AppContextM [TenantPlan]
getPlansForCurrentTenant = do
  tenantUuid <- asks currentTenantUuid
  findPlansForTenantUuid tenantUuid

createPlan :: U.UUID -> TenantPlanChangeDTO -> AppContextM TenantPlan
createPlan tenantUuid reqDto = do
  uuid <- liftIO generateUuid
  createPlanWithUuid uuid tenantUuid reqDto

createPlanWithUuid :: U.UUID -> U.UUID -> TenantPlanChangeDTO -> AppContextM TenantPlan
createPlanWithUuid uuid tenantUuid reqDto = do
  checkPermission _TENANT_PERM
  tenant <- findTenantByUuid tenantUuid
  now <- liftIO getCurrentTime
  let plan = fromChangeDTO reqDto uuid tenantUuid now now
  insertPlan plan
  recomputePlansForTenant tenant
  return plan

modifyPlan :: U.UUID -> U.UUID -> TenantPlanChangeDTO -> AppContextM TenantPlan
modifyPlan aUuid pUuid reqDto = do
  checkPermission _TENANT_PERM
  tenant <- findTenantByUuid aUuid
  plan <- findPlanByUuid pUuid
  let updatedPlan = fromChangeDTO reqDto plan.uuid plan.tenantUuid plan.createdAt plan.updatedAt
  updatePlanByUuid updatedPlan
  recomputePlansForTenant tenant
  return updatedPlan

deletePlan :: U.UUID -> U.UUID -> AppContextM ()
deletePlan aUuid pUuid = do
  checkPermission _TENANT_PERM
  tenant <- findTenantByUuid aUuid
  plan <- findPlanByUuid pUuid
  deletePlanByUuid pUuid
  recomputePlansForTenant tenant
  return ()

recomputePlansForTenants :: AppContextM ()
recomputePlansForTenants = do
  tenants <- findTenants
  traverse_ recomputePlansForTenant tenants

recomputePlansForTenant :: Tenant -> AppContextM ()
recomputePlansForTenant tenant = do
  now <- liftIO getCurrentTime
  plans <- findPlansForTenantUuid tenant.uuid
  let mActivePlan = headSafe . filter (isPlanActive now) $ plans
  -- Recompute active flag
  let active = isJust mActivePlan
  when (tenant.enabled /= active) (void $ updateTenantByUuid (tenant {enabled = active}))
  -- Recompute features & limits
  case mActivePlan of
    Just activePlan -> do
      tenantConfig <- getTenantConfigByUuid tenant.uuid
      let updatedTenantConfig = turnTestPlanFeature activePlan.test tenantConfig
      when (tenantConfig.feature /= updatedTenantConfig.feature) (void $ modifyTenantConfig updatedTenantConfig)
      recomputeLimitBundle tenant.uuid activePlan.users
      return ()
    Nothing -> return ()

-- --------------------------------
-- PRIVATE
-- --------------------------------
isPlanActive :: UTCTime -> TenantPlan -> Bool
isPlanActive now plan =
  case (plan.since, plan.until) of
    (Just since, Just until) -> since <= now && now <= until
    (Just since, Nothing) -> since <= now
    (Nothing, Just until) -> now <= until
    (Nothing, Nothing) -> True

turnTestPlanFeature :: Bool -> TenantConfig -> TenantConfig
turnTestPlanFeature enabled tenantConfig =
  tenantConfig
    { feature =
        tenantConfig.feature
          { pdfOnlyEnabled = enabled
          , pdfWatermarkEnabled = enabled
          }
    }
