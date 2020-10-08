module Wizard.Specs.API.User.List_POST
  ( list_POST
  ) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig hiding (request)
import Shared.Localization.Messages.Public
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserJM ()
import Wizard.Database.DAO.ActionKey.ActionKeyDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import SharedTest.Specs.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.User.Common

-- ------------------------------------------------------------------------
-- POST /users
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /users" $ do
    test_201 appContext
    test_400 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/users"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = userJohnCreate

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext =
  it "HTTP 201 CREATED" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 201
    let expHeaders = resCorsHeadersPlain
    let expDto = userJohnCreate
    let expBody = encode expDto
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, UserDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareUserCreateDtos resDto expDto
    -- AND: Find result in DB and compare with expectation state
    assertCountInDB findActionKeys appContext 1
    assertCountInDB findUsers appContext 2

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = do
  createInvalidJsonTest reqMethod reqUrl "lastName"
  it "HTTP 400 BAD REQUEST if email is already registered" $
    -- GIVEN: Prepare request
   do
    let reqDto = userJohnCreate & email .~ (userAlbert ^. email)
    let reqBody = encode reqDto
    -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = createValidationError [] [("email", _ERROR_VALIDATION__USER_EMAIL_UNIQUENESS $ reqDto ^. email)]
    let expBody = encode expDto
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- AND: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
