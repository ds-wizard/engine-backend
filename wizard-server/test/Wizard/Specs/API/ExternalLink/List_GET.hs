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
import WizardLib.Public.Localization.Messages.Public

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/external-link?url=https://docs.ds-wizard.org/my-link
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext = describe "GET /wizard-api/external-link" $ do
  test_302 appContext
  test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

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
      let expDto = FoundError "https://guide.ds-wizard.org/my-link"
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod "/wizard-api/external-link?url=https://guide.ds-wizard.org/my-link" reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- THEN: Compare DB with expectation
      assertCountInDB findExternalLinkUsages appContext 1

test_403 appContext =
  it "HTTP 403 FORBIDDEN" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError _ERROR_SERVICE_EXTERNAL_LINK__URL_NOT_ALLOWED
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod "/wizard-api/external-link?url=https://evil.com" reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- THEN: DB must remain empty
      assertCountInDB findExternalLinkUsages appContext 0
