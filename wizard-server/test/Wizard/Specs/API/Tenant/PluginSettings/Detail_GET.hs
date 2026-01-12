module Wizard.Specs.API.Tenant.PluginSettings.Detail_GET (
  detail_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Wizard.Database.DAO.Tenant.PluginSettings.TenantPluginSettingsDAO
import Wizard.Database.Migration.Development.Plugin.Data.PluginSettings
import Wizard.Database.Migration.Development.Plugin.Data.Plugins
import Wizard.Database.Migration.Development.Tenant.Data.TenantPluginSettings
import Wizard.Model.Context.AppContext
import Wizard.Model.Plugin.Plugin

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/tenants/current/plugin-settings/{uuid}
-- ------------------------------------------------------------------------
detail_GET :: AppContext -> SpecWith ((), Application)
detail_GET appContext =
  describe "GET /wizard-api/tenants/current/plugin-settings/{uuid}" $ do
    test_200 appContext
    test_401 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = BS.pack $ "/wizard-api/tenants/current/plugin-settings/" ++ U.toString plugin1.uuid

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = plugin1Values1
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO (insertTenantPluginSettings defaultTenantPluginSettings) appContext
      runInContextIO (insertTenantPluginSettings differentTenantPluginSettings) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
