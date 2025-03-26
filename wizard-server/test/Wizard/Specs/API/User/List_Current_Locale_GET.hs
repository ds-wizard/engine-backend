module Wizard.Specs.API.User.List_Current_Locale_GET (
  list_current_locale_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Wizard.Api.Resource.User.UserSubmissionPropsJM ()
import Wizard.Model.Context.AppContext
import WizardLib.Public.Database.Migration.Development.User.Data.Users

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/users/current/locale
-- ------------------------------------------------------------------------
list_current_locale_GET :: AppContext -> SpecWith ((), Application)
list_current_locale_GET appContext =
  describe "GET /wizard-api/users/current/locale" $ do
    test_200 appContext
    test_401 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/users/current/locale"

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
      let expDto = userLocaleEmpty
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher = ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
