module Wizard.Specs.API.Tenant.PluginSettings.Detail_PUT (
  detail_PUT,
) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Database.DAO.Tenant.PluginSettings.TenantPluginSettingsDAO
import Wizard.Database.Migration.Development.Plugin.Data.PluginSettings
import Wizard.Database.Migration.Development.Plugin.Data.Plugins
import Wizard.Database.Migration.Development.Tenant.Data.TenantPluginSettings
import Wizard.Model.Context.AppContext
import Wizard.Model.Plugin.Plugin
import Wizard.Model.Tenant.PluginSettings.TenantPluginSettings

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Config.Common
import Wizard.Specs.API.Tenant.PluginSettings.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/tenants/current/plugin-settings/{uuid}
-- ------------------------------------------------------------------------
detail_PUT :: AppContext -> SpecWith ((), Application)
detail_PUT appContext =
  describe "PUT /wizard-api/tenants/current/plugin-settings/{uuid}" $ do
    test_200 appContext
    test_401 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = BS.pack $ "/wizard-api/tenants/current/plugin-settings/" ++ U.toString plugin1.uuid

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = plugin1Values1Edited

reqBody = A.encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = defaultTenantPluginSettingsEdited.values
      -- AND: Run migrations
      runInContextIO (insertTenantPluginSettings defaultTenantPluginSettings) appContext
      runInContextIO (insertTenantPluginSettings differentTenantPluginSettings) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, A.Value)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareDtos resBody expDto
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfTenantPluginSettingsInDB appContext defaultTenantPluginSettingsEdited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
