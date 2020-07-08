module Wizard.Specs.API.Cache.List_DELETE
  ( list_delete
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import qualified Data.Cache as C
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig hiding (request)
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

import SharedTest.Specs.Common
import Wizard.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /caches
-- ------------------------------------------------------------------------
list_delete :: AppContext -> SpecWith ((), Application)
list_delete appContext =
  describe "GET /caches" $ do
    test_204 appContext
    test_401 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = "/caches"

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
     -- AND: Run migrations
    liftIO $ C.insert (appContext ^. cache . knowledgeModel) 123 undefined
    liftIO $ C.insert (appContext ^. cache . questionnaireReportIndications) 456 undefined
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
    -- AND: Check empty cache
    kmCacheSize <- liftIO . C.size $ appContext ^. cache . knowledgeModel
    liftIO $ kmCacheSize `shouldBe` 0
    iCacheSize <- liftIO . C.size $ appContext ^. cache . questionnaireReportIndications
    liftIO $ iCacheSize `shouldBe` 0

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
