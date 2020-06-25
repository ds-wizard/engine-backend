module Wizard.Specs.API.User.Detail_Password_Hash_PUT
  ( detail_password_hash_PUT
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig hiding (request)
import Wizard.Database.DAO.ActionKey.ActionKeyDAO
import Wizard.Database.Migration.Development.ActionKey.Data.ActionKeys
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

import SharedTest.Specs.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.User.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /users/{userId}/password?hash={hash}
-- ------------------------------------------------------------------------
detail_password_hash_PUT :: AppContext -> SpecWith ((), Application)
detail_password_hash_PUT appContext =
  describe "PUT /users/{userId}/password?hash={hash}" $ do
    test_204 appContext
    test_400 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66/password?hash=1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"

reqHeaders = [reqCtHeader]

reqDto = userPassword

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 appContext =
  it "HTTP 204 NO CONTENT" $
    -- GIVEN: Prepare DB
   do
    eitherActionKey <- runInContextIO (insertActionKey forgPassActionKey) appContext
    -- AND: Prepare expectation
    let expStatus = 204
    let expHeaders = resCorsHeaders
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals ""}
    response `shouldRespondWith` responseMatcher
    -- AND: Find result in DB and compare with expectation state
    assertPasswordOfUserInDB appContext userAlbert (userPassword ^. password)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = do
  createInvalidJsonTest reqMethod reqUrl "password"
  it "HTTP 400 BAD REQUEST when hash is not provided" $
    -- GIVEN: Prepare url
   do
    let reqUrlWithoutHash = "/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66/password"
    -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = resCorsHeaders
    let expDto = createUserError _ERROR_SERVICE_USER__REQUIRED_ADMIN_ROLE_OR_HASH_IN_QUERY_PARAMS
    let expBody = encode expDto
    -- WHEN: Call API
    response <- request reqMethod reqUrlWithoutHash reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
    -- AND: Find result in DB and compare with expectation state
    assertPasswordOfUserInDB appContext userAlbert "password"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest
    reqMethod
    "/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66/password?hash=e565b3da-70bf-4991-ad47-4209514b3a67"
    reqHeaders
    reqBody
    "actionKey"
    "e565b3da-70bf-4991-ad47-4209514b3a67"
