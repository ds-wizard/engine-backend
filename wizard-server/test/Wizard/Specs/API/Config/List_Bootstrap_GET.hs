module Wizard.Specs.API.Config.List_Bootstrap_GET (
  list_bootstrap_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Wizard.Api.Resource.Config.ClientConfigJM ()
import Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.Client.ClientConfigMapper
import WizardLib.Public.Database.Migration.Development.Tenant.Data.TenantConfigs

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/configs/bootstrap
-- ------------------------------------------------------------------------
list_bootstrap_GET :: AppContext -> SpecWith ((), Application)
list_bootstrap_GET appContext = describe "GET /wizard-api/configs/bootstrap" $ test_200 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/configs/bootstrap"

reqHeadersT authHeaders = authHeaders

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK (anonymous)" appContext [] Nothing
  create_test_200 "HTTP 200 OK (authenticated)" appContext [reqAuthHeader] (Just userAlbertProfile)

create_test_200 title appContext authHeaders mUserProfile =
  it title $
    -- GIVEN: Prepare variables
    do
      let reqHeaders = reqHeadersT authHeaders
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toClientConfigDTO appContext.serverConfig defaultOrganization defaultAuthentication defaultPrivacyAndSupport defaultDashboardAndLoginScreen defaultLookAndFeel defaultRegistry defaultProject defaultSubmission defaultFeatures defaultOwl mUserProfile [] defaultTenant
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
