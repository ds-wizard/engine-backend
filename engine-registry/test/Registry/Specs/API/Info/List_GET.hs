module Registry.Specs.API.Info.List_GET
  ( list_get
  ) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Registry.Api.Resource.Info.InfoDTO
import Registry.Api.Resource.Info.InfoJM ()
import Registry.Model.Context.AppContext

import Registry.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /
-- ------------------------------------------------------------------------
list_get :: AppContext -> SpecWith Application
list_get appContext = describe "GET /" $ test_200 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/"

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
    let expDto =
          InfoDTO {_infoDTOName = "Registry", _infoDTOVersion = "1.0.0", _infoDTOBuiltAt = "2017/10/25 19:50:20Z"}
    let expBody = encode expDto
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
