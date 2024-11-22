module Wizard.Specs.API.Tenant.Limit.List_PUT (
  list_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Model.Context.AppContext
import WizardLib.Public.Api.Resource.Tenant.Usage.WizardUsageDTO
import WizardLib.Public.Api.Resource.Tenant.Usage.WizardUsageJM ()
import WizardLib.Public.Database.Migration.Development.Tenant.Data.TenantLimitBundles
import WizardLib.Public.Database.Migration.Development.Tenant.Data.TenantUsages

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/tenants/{tenantUuid}/limits
-- ------------------------------------------------------------------------
list_PUT :: AppContext -> SpecWith ((), Application)
list_PUT appContext =
  describe "PUT /wizard-api/tenants/{tenantUuid}/limits" $ do
    test_200 appContext
    test_400_invalid_json appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/tenants/00000000-0000-0000-0000-000000000000/limits"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = tenantLimitBundleChange

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
      let expDto = defaultUsageEdited
      let expType (a :: WizardUsageDTO) = a
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertResponseWithoutFields expStatus expHeaders expDto expType response ["updatedAt"]

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_json appContext = createInvalidJsonTest reqMethod reqUrl "uuid"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "TENANT_PERM"
