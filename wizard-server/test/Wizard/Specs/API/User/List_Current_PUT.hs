module Wizard.Specs.API.User.List_Current_PUT (
  list_current_PUT,
) where

import Data.Aeson (encode)
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserJM ()
import Wizard.Api.Resource.User.UserProfileChangeDTO
import Wizard.Api.Resource.User.UserProfileChangeJM ()
import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User
import Wizard.Service.User.UserMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.User.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/users/current
-- ------------------------------------------------------------------------
list_current_PUT :: AppContext -> SpecWith ((), Application)
list_current_PUT appContext =
  describe "PUT /wizard-api/users/current" $ do
    test_200 appContext
    test_400 appContext
    test_401 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/users/current"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = userAlbertEditedChange

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
      let expDto = toDTO userAlbertEdited
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, UserDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareUserDtos resDto expDto
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfUserInDB appContext userAlbertEdited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = do
  createInvalidJsonTest reqMethod reqUrl "password"
  it "HTTP 400 BAD REQUEST if email is already registered" $
    -- GIVEN: Prepare request
    do
      let reqDto = userIsaacProfileChange {email = userIsaac.email} :: UserProfileChangeDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.singleton "email" [_ERROR_VALIDATION__USER_EMAIL_UNIQUENESS $ reqDto.email])
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
