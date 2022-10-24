module Wizard.Specs.API.Token.List_POST
  ( list_post
  ) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig hiding (request)
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Model.Error.Error
import Wizard.Api.Resource.UserToken.UserTokenDTO
import Wizard.Database.Migration.Development.User.Data.UserTokens
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common

-- ------------------------------------------------------------------------
-- POST /tokens
-- ------------------------------------------------------------------------
list_post :: AppContext -> SpecWith ((), Application)
list_post appContext =
  describe "POST /tokens" $ do
    test_201 appContext
    test_400 appContext

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
test_201 appContext =
  it "HTTP 201 CREATED" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 201
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, UserTokenDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    liftIO $ (resBody ^. token) `shouldStartWith` "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = do
  createInvalidJsonTest reqMethod reqUrl "password"
  it "HTTP 400 BAD REQUEST when invalid creadentials are provided" $
     -- GIVEN: Prepare request
   do
    let reqDto = albertCreateToken & email .~ "albert.einstein@example.com2"
    let reqBody = encode reqDto
     -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = UserError _ERROR_SERVICE_TOKEN__INCORRECT_EMAIL_OR_PASSWORD
    let expBody = encode expDto
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
