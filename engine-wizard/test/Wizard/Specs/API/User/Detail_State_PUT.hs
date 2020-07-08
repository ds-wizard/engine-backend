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
import Wizard.Database.DAO.ActionKey.ActionKeyDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.Migration.Development.ActionKey.Data.ActionKeys
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Context.AppContext

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
    test_404 appContext

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
test_400 appContext = createInvalidJsonTest reqMethod reqUrl "active"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest
    reqMethod
    "/users/dc9fe65f-748b-47ec-b30c-d255bbac64a0/state?hash="
    reqHeaders
    reqBody
    "user"
    "dc9fe65f-748b-47ec-b30c-d255bbac64a0"
