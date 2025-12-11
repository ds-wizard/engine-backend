module Wizard.Specs.API.Tenant.Config.List_PUT (
  list_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Api.Resource.Tenant.Config.TenantConfigChangeDTO hiding (request)
import Wizard.Api.Resource.Tenant.Config.TenantConfigChangeJM ()
import Wizard.Api.Resource.Tenant.Config.TenantConfigJM ()
import Wizard.Database.DAO.Tenant.Config.TenantConfigSubmissionDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs
import Wizard.Model.Context.AppContext
import Wizard.Model.Tenant.Config.TenantConfig hiding (request)

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Config.Common
import Wizard.Specs.Common

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

reqDto = defaultTenantConfigChangeDto {project = editedProjectChangeDto} :: TenantConfigChangeDTO

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
      let expDto = defaultTenantConfig {project = editedProject} :: TenantConfig
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO (insertOrUpdateConfigSubmissionService defaultSubmissionService) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, TenantConfig)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareDtos resBody expDto
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfTenantConfigProjectInDB appContext editedProject

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
