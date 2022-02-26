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

reqHeaders = [reqCtHeader]

reqDto = appCreateDto

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
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, AppDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    -- AND: Find result in DB and compare with expectation state
    (Right app) <- runInContextIO (findAppByClientUrl $ resDto ^. clientUrl) appContext
    let updatedAppContext = appContext & appUuid .~ (app ^. uuid)
    assertCountInDB findUsers updatedAppContext 1
    assertCountInDB findActionKeys updatedAppContext 1

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = do
  createInvalidJsonTest reqMethod reqUrl "lastName"
  it "HTTP 400 BAD REQUEST if appId is already used" $
    -- GIVEN: Prepare request
   do
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
