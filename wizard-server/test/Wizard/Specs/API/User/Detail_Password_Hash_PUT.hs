module Wizard.Specs.API.User.Detail_Password_Hash_PUT (
  detail_password_hash_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.ActionKey.Database.DAO.ActionKey.ActionKeyDAO
import Wizard.Api.Resource.User.UserPasswordDTO
import Wizard.Database.Migration.Development.ActionKey.Data.ActionKeys
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.User.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /users/{uuid}/password?hash={hash}
-- ------------------------------------------------------------------------
detail_password_hash_PUT :: AppContext -> SpecWith ((), Application)
detail_password_hash_PUT appContext =
  describe "PUT /users/{uuid}/password?hash={hash}" $ do
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
    "/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66/password?hash=c996414a-b51d-4c8c-bc10-5ee3dab85fa8"
    reqHeaders
    reqBody
    "action_key"
    [("hash", "c996414a-b51d-4c8c-bc10-5ee3dab85fa8")]
