module Wizard.Specs.API.App.List_POST
  ( list_POST
  ) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (encode)
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig hiding (request)
import Shared.Model.Error.Error
import Wizard.Api.Resource.App.AppDTO
import Wizard.Api.Resource.App.AppJM ()
import Wizard.Database.DAO.ActionKey.ActionKeyDAO
import Wizard.Database.DAO.App.AppDAO
import Wizard.Database.DAO.PersistentCommand.PersistentCommandDAO
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /apps
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /apps" $ do
    test_201 appContext
    test_400 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/apps"

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqDtoT dto = dto

reqBodyT dto = encode (reqDtoT dto)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext = do
  create_test_201 "HTTP 201 CREATED (anonymous)" appContext appCreateDto [] 2 False
  create_test_201 "HTTP 201 CREATED (admin)" appContext appCreateDto [reqAuthHeader] 1 True

create_test_201 title appContext reqDto authHeaders persistentCommandCount userActive =
  it title $
     -- GIVEN: Prepare request
   do
    let reqHeaders = reqHeadersT authHeaders
    let reqBody = reqBodyT reqDto
    -- GIVEN: Prepare expectation
    let expStatus = 201
    let expHeaders = resCorsHeadersPlain
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, AppDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    -- AND: Find result in DB and compare with expectation state
    (Right app) <- runInContextIO (findAppByClientUrl $ resDto ^. clientUrl) appContext
    let updatedAppContext = appContext & appUuid .~ (app ^. uuid)
    (Right [user]) <- runInContextIO findUsers updatedAppContext
    liftIO $ user ^. active `shouldBe` userActive
    assertCountInDB findActionKeys updatedAppContext 1
    assertCountInDB findPersistentCommands updatedAppContext persistentCommandCount

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = do
  createInvalidJsonTest reqMethod reqUrl "lastName"
  create_test_400_app_id_uniqueness "HTTP 400 BAD REQUEST if appId is already used (anonymous)" appContext []
  create_test_400_app_id_uniqueness "HTTP 400 BAD REQUEST if appId is already used (admin)" appContext [reqAuthHeader]

create_test_400_app_id_uniqueness title appContext authHeaders =
  it title $
    -- GIVEN: Prepare request
   do
    let reqHeaders = reqHeadersT authHeaders
    let reqDto = appCreateDto & appId .~ "default"
    let reqBody = encode reqDto
    -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = ValidationError [] (M.singleton "appId" [_ERROR_VALIDATION__APP_ID_UNIQUENESS])
    let expBody = encode expDto
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- AND: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
