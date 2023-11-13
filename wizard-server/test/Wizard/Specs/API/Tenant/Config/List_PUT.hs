module Wizard.Specs.API.Tenant.Config.List_PUT (
  list_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Api.Resource.Tenant.Config.TenantConfigJM ()
import Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Config.TenantConfig hiding (request)
import Wizard.Service.Tenant.Config.ConfigMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Config.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/tenants/current/config
-- ------------------------------------------------------------------------
list_PUT :: AppContext -> SpecWith ((), Application)
list_PUT appContext =
  describe "PUT /wizard-api/tenants/current/config" $ do
    test_200 appContext
    test_400_invalid_json appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/tenants/current/config"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = toChangeDTO editedTenantConfig

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
      let expDto = editedTenantConfig
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, TenantConfig)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareDtos resBody expDto
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfTenantConfigInDB appContext editedTenantConfig

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
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "CFG_PERM"
