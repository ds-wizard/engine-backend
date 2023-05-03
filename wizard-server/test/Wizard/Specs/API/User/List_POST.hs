module Wizard.Specs.API.User.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.ActionKey.Database.DAO.ActionKey.ActionKeyDAO
import Shared.ActionKey.Model.ActionKey.ActionKey
import Shared.Common.Model.Error.Error
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Api.Resource.User.UserCreateDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserJM ()
import Wizard.Database.DAO.User.UserDAO
import qualified Wizard.Database.Migration.Development.ActionKey.ActionKeyMigration as ACK
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Localization.Messages.Public
import Wizard.Model.ActionKey.ActionKeyType
import Wizard.Model.Context.AppContext
import Wizard.Model.User.User

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.User.Common
import Wizard.Specs.Common

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

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqDtoT dto = dto

reqBodyT dto = encode (reqDtoT dto)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext = do
  create_test_201 "HTTP 201 CREATED (anonymous)" appContext userJohnCreate userJohnCreateDS [] 1 False
  create_test_201 "HTTP 201 CREATED (admin)" appContext userJohnCreate userJohnCreate [reqAuthHeader] 0 True

create_test_201 title appContext reqDto expDto authHeaders persistentCommandCount userActive =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeaders
      let reqBody = reqBodyT reqDto
      -- GIVEN: Prepare expectation
      let expStatus = 201
      let expHeaders = resCorsHeadersPlain
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO ACK.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, UserDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareUserCreateDtos resDto expDto userActive
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB (findActionKeys :: AppContextM [ActionKey U.UUID ActionKeyType]) appContext 1
      assertCountInDB findUsers appContext 2
      assertCountInDB (findPersistentCommands :: AppContextM [PersistentCommand U.UUID]) appContext persistentCommandCount

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = do
  createInvalidJsonTest reqMethod reqUrl "lastName"
  create_test_400_email_uniqueness "HTTP 400 BAD REQUEST if email is already registered (anonymous)" appContext []
  create_test_400_email_uniqueness
    "HTTP 400 BAD REQUEST if email is already registered (admin)"
    appContext
    [reqAuthHeader]

create_test_400_email_uniqueness title appContext authHeaders =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeaders
      let reqDto = userJohnCreate {email = userAlbert.email} :: UserCreateDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ValidationError [] (M.singleton "email" [_ERROR_VALIDATION__USER_EMAIL_UNIQUENESS $ reqDto.email])
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
