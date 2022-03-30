module Wizard.Specs.API.App.Plan.List_POST
  ( list_POST
  ) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Database.Migration.Development.Plan.Data.AppPlans
import Wizard.Model.Context.AppContext
import Wizard.Model.Plan.AppPlan
import Wizard.Service.Plan.AppPlanMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.App.Plan.Common
import Wizard.Specs.API.Common

-- ------------------------------------------------------------------------
-- POST /apps/{appUuid}/plans
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /apps/{appUuid}/plans" $ do
    test_201 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/apps/00000000-0000-0000-0000-000000000000/plans"

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
    let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, AppPlan)
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
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "APP_PERM"
