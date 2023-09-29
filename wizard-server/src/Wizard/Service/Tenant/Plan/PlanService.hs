module Wizard.Service.Tenant.Plan.PlanService (
  getPlansForCurrentTenant,
  createPlan,
  createPlanWithUuid,
  modifyPlan,
  deletePlan,
  recomputePlansForTenants,
) where

import Control.Monad (void, when)
import Control.Monad.Reader (liftIO)
import Data.Foldable (traverse_)
import Data.Maybe (isJust)
import Data.Time
import qualified Data.UUID as U
import Prelude hiding (until)

import Shared.Common.Util.List
import Wizard.Database.DAO.Tenant.TenantDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Tenant
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.Limit.LimitService
import WizardLib.Public.Api.Resource.Tenant.Plan.TenantPlanChangeDTO
import WizardLib.Public.Database.DAO.Tenant.TenantPlanDAO
import WizardLib.Public.Model.Tenant.Plan.TenantPlan
import WizardLib.Public.Service.Tenant.Plan.PlanService

createPlan :: U.UUID -> TenantPlanChangeDTO -> AppContextM TenantPlan
createPlan = createPlan' findTenantByUuid recomputePlansForTenant

createPlanWithUuid :: U.UUID -> U.UUID -> TenantPlanChangeDTO -> AppContextM TenantPlan
createPlanWithUuid = createPlanWithUuid' findTenantByUuid recomputePlansForTenant

modifyPlan :: U.UUID -> U.UUID -> TenantPlanChangeDTO -> AppContextM TenantPlan
modifyPlan = modifyPlan' findTenantByUuid recomputePlansForTenant

deletePlan :: U.UUID -> U.UUID -> AppContextM ()
deletePlan = deletePlan' findTenantByUuid recomputePlansForTenant

recomputePlansForTenants :: AppContextM ()
recomputePlansForTenants = do
  tenants <- findTenants
  traverse_ recomputePlansForTenant tenants

recomputePlansForTenant :: Tenant -> AppContextM ()
recomputePlansForTenant tenant = do
  now <- liftIO getCurrentTime
  plans <- findTenantPlansForTenantUuid tenant.uuid
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
