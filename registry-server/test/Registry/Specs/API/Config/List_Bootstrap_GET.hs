module Registry.Specs.API.Config.List_Bootstrap_GET (
  list_bootstrap_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Registry.Api.Resource.Config.ClientConfigJM ()
import Registry.Model.Context.AppContext
import Registry.Service.Config.Client.ClientConfigMapper

import SharedTest.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /configs/bootstrap
-- ------------------------------------------------------------------------
list_bootstrap_GET :: AppContext -> SpecWith ((), Application)
list_bootstrap_GET appContext = describe "GET /configs/bootstrap" $ test_200 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/configs/bootstrap"

reqHeaders = []

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
      let expDto = toClientConfigDTO appContext.serverConfig
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
