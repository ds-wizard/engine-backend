module Wizard.Specs.API.User.List_Current_PUT
  ( list_current_PUT
  ) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher

import LensesConfig hiding (request)
import Shared.Localization.Messages.Public
import Wizard.Api.Resource.User.UserProfileChangeJM ()
import Wizard.Api.Resource.User.UserProfileDTO
import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Model.Context.AppContext

import SharedTest.Specs.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.User.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /users/current
-- ------------------------------------------------------------------------
list_current_PUT :: AppContext -> SpecWith Application
list_current_PUT appContext =
  describe "PUT /users/current" $ do
    test_200 appContext
    test_400_invalid_json appContext
    test_401 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/users/current"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = userIsaacProfileChange

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 200
    let expHeaders = resCorsHeadersPlain
    let expDto = userAlbertProfileEdited
    let expBody = encode expDto
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, UserProfileDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareUserDtos resDto expDto
    -- AND: Find result in DB and compare with expectation state
    assertExistenceOfUserInDB appContext userAlbertEdited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_json appContext = do
  createInvalidJsonTest reqMethod reqUrl [HJ.json| { } |] "password"
  it "HTTP 400 BAD REQUEST if email is already registered" $
    -- GIVEN: Prepare request
   do
    let reqDto = userIsaacProfileChange & email .~ (userIsaac ^. email)
    let reqBody = encode reqDto
    -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = createValidationError [] [("email", _ERROR_VALIDATION__USER_EMAIL_UNIQUENESS $ reqDto ^. email)]
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO U_Migration.runMigration appContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- AND: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody
