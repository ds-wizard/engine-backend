module Wizard.Specs.API.Tenant.Plan.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Model.Context.AppContext
import WizardLib.Public.Database.Migration.Development.Tenant.Data.TenantPlans
import WizardLib.Public.Model.Tenant.Plan.TenantPlan
import WizardLib.Public.Service.Tenant.Plan.PlanMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Tenant.Plan.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/tenants/{tenantUuid}/plans
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /wizard-api/tenants/{tenantUuid}/plans" $ do
    test_201 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/tenants/00000000-0000-0000-0000-000000000000/plans"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = toChangeDTO standardPlan

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext = do
  it "HTTP 201 CREATED" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCorsHeadersPlain
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, TenantPlan)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      -- AND: Find result in DB and compare with expectation state
      comparePlanDtos resDto standardPlan
      assertExistenceOfPlanInDB appContext standardPlan

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod reqUrl "lastName"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "TENANT_PERM"
