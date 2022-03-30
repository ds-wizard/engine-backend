module Wizard.Specs.API.Domain.Detail_GET
  ( detail_GET
  ) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common ()

-- ------------------------------------------------------------------------
-- GET /apps
-- ------------------------------------------------------------------------
detail_GET :: AppContext -> SpecWith ((), Application)
detail_GET appContext =
  describe "GET /domains/{appId}" $ do
    test_204 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/domains/default"

reqHeaders = []

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 appContext = do
  it "HTTP 204 NO CONTENT" $
       -- GIVEN: Prepare request
   do
    let expStatus = 204
    let expHeaders = resCorsHeaders
    let expBody = ""
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- AND: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest
    reqMethod
    "/domains/non-existing-domain"
    reqHeaders
    reqBody
    "app"
    [("app_id", "non-existing-domain")]
