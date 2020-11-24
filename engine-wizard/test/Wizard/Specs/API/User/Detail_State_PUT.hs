module Wizard.Specs.API.User.Detail_State_PUT
  ( detail_state_PUT
  ) where

import Control.Lens ((&), (.~))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig hiding (request)
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Database.DAO.ActionKey.ActionKeyDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.Migration.Development.ActionKey.Data.ActionKeys
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.User.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /users/{userId}/state?hash={hash}
-- ------------------------------------------------------------------------
detail_state_PUT :: AppContext -> SpecWith ((), Application)
detail_state_PUT appContext =
  describe "PUT /users/{userId}/state?hash={hash}" $ do
    test_200 appContext
    test_400 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66/state?hash=1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"

reqHeaders = [reqCtHeader]

reqDto = userState

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 200
    let expHeaders = resCorsHeaders
    let expDto = reqDto
    let expBody = encode expDto
     -- AND: Prepare DB
    runInContextIO (insertActionKey regActionKey) appContext
    runInContextIO (updateUserById (userAlbert & active .~ False)) appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertCountInDB findActionKeys appContext 0
    assertExistenceOfUserInDB appContext userAlbert

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = do
  createInvalidJsonTest reqMethod reqUrl "active"
  it "HTTP 400 BAD REQUEST when hash is not provided" $
    -- GIVEN: Prepare url
   do
    let reqUrlWithoutHash = "/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66/state"
    -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = resCorsHeaders
    let expDto = UserError _ERROR_VALIDATION__HASH_ABSENCE
    let expBody = encode expDto
     -- AND: Prepare DB
    runInContextIO (insertActionKey regActionKey) appContext
    runInContextIO (updateUserById (userAlbert & active .~ False)) appContext
    -- WHEN: Call API
    response <- request reqMethod reqUrlWithoutHash reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
    -- AND: Find result in DB and compare with expectation state
    assertExistenceOfUserInDB appContext (userAlbert & active .~ False)
  it "HTTP 400 BAD REQUEST when hash is not in DB" $
   -- GIVEN: Prepare request
   do
    let reqUrl = "/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66/state?hash=c996414a-b51d-4c8c-bc10-5ee3dab85fa8"
    let reqHeaders = [reqCtHeader]
    -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = resCorsHeaders
    let expDto = UserError _ERROR_VALIDATION__HASH_ABSENCE
    let expBody = encode expDto
     -- AND: Prepare DB
    runInContextIO (insertActionKey regActionKey) appContext
    runInContextIO (updateUserById (userAlbert & active .~ False)) appContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
    -- AND: Find result in DB and compare with expectation state
    assertExistenceOfUserInDB appContext (userAlbert & active .~ False)
