module Wizard.Specs.API.OpenIdClient.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Wizard.Model.Context.AppContext
import WizardLib.Public.Database.Migration.Development.OpenId.Data.OpenIdClients
import qualified WizardLib.Public.Database.Migration.Development.OpenId.OpenIdClientMigration as OPENID_Migration

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/open-id-clients
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext =
  describe "GET /wizard-api/open-id-clients" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/open-id-clients"

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
      let expDto = [defaultOpenIdClientSimple]
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO OPENID_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [] reqBody "SettingsManageRolePermission"
