module Wizard.Specs.API.Token.List_POST
  ( list_post
  ) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Wizard.Api.Resource.Token.TokenDTO
import Wizard.Database.Migration.Development.Token.Data.Tokens
import Wizard.LensesConfig
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

import SharedTest.Specs.Common
import Wizard.Specs.API.Common

-- ------------------------------------------------------------------------
-- POST /tokens
-- ------------------------------------------------------------------------
list_post :: AppContext -> SpecWith Application
list_post appContext =
  describe "POST /tokens" $ do
    test_201 appContext
    test_400_invalid_json appContext
    test_401_bad_credentials appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/tokens"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = albertCreateToken

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext = do
  it "HTTP 201 CREATED" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 201
    let expHeaders = [resCtHeaderPlain] ++ resCorsHeadersPlain
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, TokenDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    liftIO $ (resBody ^. token) `shouldStartWith` "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_json appContext =
  createInvalidJsonTest reqMethod reqUrl [HJ.json| { email: "albert.einstein@example.com" } |] "password"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401_bad_credentials appContext = do
  it "HTTP 401 UNAUTHORIZED when invalid creadentials are provided" $
     -- GIVEN: Prepare request
   do
    let reqDto = albertCreateToken & email .~ "albert.einstein@example.com2"
    let reqBody = encode reqDto
     -- AND: Prepare expectation
    let expStatus = 401
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = createUnauthorizedError _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD
    let expBody = encode expDto
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
