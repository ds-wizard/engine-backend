module Wizard.Specs.API.ExternalLink.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Model.Error.Error
import Wizard.Model.Context.AppContext
import WizardLib.Public.Database.DAO.ExternalLink.ExternalLinkUsageDAO

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/external-link?url=http://example.com/my-link
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext = describe "GET /wizard-api/external-link?url=http://example.com/my-link" $ test_302 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/external-link?url=http://example.com/my-link"

reqHeaders = []

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_302 appContext =
  it "HTTP 302 FOUND" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 302
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = FoundError "http://example.com/my-link"
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- THEN: Compare DB with expectation
      assertCountInDB findExternalLinkUsages appContext 1
