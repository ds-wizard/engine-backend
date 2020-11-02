module Wizard.Specs.API.Swagger.List_GET
  ( list_get
  ) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /swaggers
-- ------------------------------------------------------------------------
list_get :: AppContext -> SpecWith ((), Application)
list_get appContext = describe "GET /swagger.json" $ test_200 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/swagger.json"

reqHeaders = [reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 200
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, String)
    assertResStatus status expStatus
