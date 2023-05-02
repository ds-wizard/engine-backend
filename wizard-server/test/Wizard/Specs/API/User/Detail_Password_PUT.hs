module Wizard.Specs.API.User.Detail_Password_PUT (
  detail_password_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Wizard.Api.Resource.User.UserPasswordDTO
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.User.Common

-- ------------------------------------------------------------------------
-- PUT /users/{userId}/password
-- ------------------------------------------------------------------------
detail_password_PUT :: AppContext -> SpecWith ((), Application)
detail_password_PUT appContext =
  describe "PUT /users/{userId}/password" $ do
    test_204 appContext
    test_400 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66/password"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = userPassword

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 appContext =
  it "HTTP 204 NO CONTENT" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 204
      let expHeaders = resCorsHeaders
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertPasswordOfUserInDB appContext userAlbert userPassword.password

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod reqUrl "password"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/users/dc9fe65f-748b-47ec-b30c-d255bbac64a0/password"
    reqHeaders
    reqBody
    "user_entity"
    [("uuid", "dc9fe65f-748b-47ec-b30c-d255bbac64a0")]
