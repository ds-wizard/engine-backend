module Wizard.Specs.API.Domain.Detail_GET (
  detail_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common ()

-- ------------------------------------------------------------------------
-- GET /wizard-api/apps
-- ------------------------------------------------------------------------
detail_GET :: AppContext -> SpecWith ((), Application)
detail_GET appContext =
  describe "GET /wizard-api/wizard-api/domains?check-domain={appId}" $ do
    test_204 appContext
    test_400 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT appId = BS.pack $ "/wizard-api/domains?check-domain=" ++ appId

reqHeaders = []

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 appContext = do
  it "HTTP 204 NO CONTENT" $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT "new-domain"
      -- AND: Prepare expectation
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
test_400 appContext = do
  create_test_400_app_id
    "HTTP 400 BAD REQUEST if appId has forbidden characters"
    appContext
    "-forbidden-"
    (_ERROR_VALIDATION__FORBIDDEN_CHARACTERS "-forbidden-")
  create_test_400_app_id
    "HTTP 400 BAD REQUEST if appId is already used"
    appContext
    "default"
    _ERROR_VALIDATION__APP_ID_UNIQUENESS

create_test_400_app_id title appContext appId errorMessage =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT appId
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.singleton "appId" [errorMessage])
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
