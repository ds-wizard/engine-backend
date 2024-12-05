module Wizard.Specs.API.Tenant.Usage.Detail_Wizard_GET (
  detail_wizard_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Wizard.Model.Context.AppContext
import WizardLib.Public.Api.Resource.Tenant.Usage.WizardUsageJM ()
import WizardLib.Public.Database.Migration.Development.Tenant.Data.TenantUsages

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/tenants/00000000-0000-0000-0000-000000000000/usages/wizard
-- ------------------------------------------------------------------------
detail_wizard_GET :: AppContext -> SpecWith ((), Application)
detail_wizard_GET appContext =
  describe "GET /wizard-api/tenants/00000000-0000-0000-0000-000000000000/usages/wizard" $ do
    test_200 appContext
    test_401 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/tenants/00000000-0000-0000-0000-000000000000/usages/wizard"

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
      let expDto = defaultUsage
      let expBody = encode expDto
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
