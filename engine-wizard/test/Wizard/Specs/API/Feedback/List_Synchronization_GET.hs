module Wizard.Specs.API.Feedback.List_Synchronization_GET
  ( list_synchronization_get
  ) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

import SharedTest.Specs.Common
import Wizard.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /feedbacks/synchronization
-- ------------------------------------------------------------------------
list_synchronization_get :: AppContext -> SpecWith Application
list_synchronization_get appContext =
  describe "GET /feedbacks/synchronization" $ do
    test_204 appContext
    test_401 appContext

--  test_204 appContext -- Disable due to slow running
-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/feedbacks/synchronization"

reqHeaders = [reqServiceHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 appContext =
  it "HTTP 204 NO CONTENT" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 204
    let expHeaders = resCorsHeaders
    let expBody = ""
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext =
  it "HTTP 401 UNAUTHORIZED" $
     -- GIVEN: Prepare request
   do
    let reqHeaders = []
     -- GIVEN: Prepare expectation
    let expStatus = 401
    let expHeaders = resCorsHeaders
    let expDto = createUnauthorizedError _ERROR_SERVICE_TOKEN__UNABLE_TO_GET_OR_VERIFY_SERVICE_TOKEN
    let expBody = encode expDto
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
