module Wizard.Specs.API.Tenant.Plan.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Database.Migration.Development.Tenant.Data.TenantPlans
import qualified Wizard.Database.Migration.Development.Tenant.TenantMigration as TNT_Migration
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Plan.TenantPlan
import Wizard.Service.Tenant.Plan.PlanMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Tenant.Plan.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/tenants/{tenantUuid}/plans/{planUuid}
-- ------------------------------------------------------------------------
detail_PUT :: AppContext -> SpecWith ((), Application)
detail_PUT appContext =
  describe "PUT /wizard-api/tenants/{tenantUuid}/plans/{planUuid}" $ do
    test_200 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/tenants/00000000-0000-0000-0000-000000000000/plans/f54f7b16-804b-439f-b8db-abbae171f18b"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = toChangeDTO standardPlanEdited

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runInContextIO TNT_Migration.runPlanMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, TenantPlan)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      comparePlanDtos resDto standardPlanEdited
      assertExistenceOfPlanInDB appContext standardPlanEdited

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
