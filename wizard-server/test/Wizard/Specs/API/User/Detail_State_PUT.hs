module Wizard.Specs.API.User.Detail_State_PUT (
  detail_state_PUT,
) where

import Data.Aeson (encode)
import Data.Time (getCurrentTime)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.UserEmailLink.Database.DAO.UserEmailLink.UserEmailLinkDAO
import Shared.UserEmailLink.Model.UserEmailLink.UserEmailLink
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Database.Migration.Development.UserEmailLink.Data.UserEmailLinks
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User
import Wizard.Model.UserEmailLink.UserEmailLinkType

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.User.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/users/{uuid}/state?hash={hash}
-- ------------------------------------------------------------------------
detail_state_PUT :: AppContext -> SpecWith ((), Application)
detail_state_PUT appContext =
  describe "PUT /wizard-api/users/{uuid}/state?hash={hash}" $ do
    test_200 appContext
    test_400 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66/state?hash=1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"

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
      now <- liftIO getCurrentTime
      runInContextIO (insertUserEmailLink (registrationUserEmailLink {createdAt = now})) appContext
      runInContextIO (updateUserByUuid (userAlbert {active = False})) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB (findUserEmailLinks :: AppContextM [UserEmailLink U.UUID UserEmailLinkType]) appContext 0
      assertExistenceOfUserInDB appContext userAlbert

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod reqUrl "active"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/users/ec6f8e90-2a91-49ec-aa3f-9eab2267fc66/state?hash=c996414a-b51d-4c8c-bc10-5ee3dab85fa8"
    reqHeaders
    reqBody
    "user_email_link"
    [("hash", "c996414a-b51d-4c8c-bc10-5ee3dab85fa8")]
