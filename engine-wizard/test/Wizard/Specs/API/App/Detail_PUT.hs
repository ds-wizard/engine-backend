module Wizard.Specs.API.App.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Model.Error.Error
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Localization.Messages.Public
import Wizard.Model.App.App
import Wizard.Model.Context.AppContext
import Wizard.Service.App.AppMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.App.Common
import Wizard.Specs.API.Common

-- ------------------------------------------------------------------------
-- PUT /apps/{appId}
-- ------------------------------------------------------------------------
detail_PUT :: AppContext -> SpecWith ((), Application)
detail_PUT appContext =
  describe "PUT /apps/{appId}" $ do
    test_200 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/apps/d9e73946-faa6-449d-83e4-2e38371b7bfa"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = toChangeDTO differentAppEdited

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, App)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareAppDtos resDto differentAppEdited
      assertExistenceOfAppInDB appContext differentAppEdited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = do
  createInvalidJsonTest reqMethod reqUrl "lastName"
  it "HTTP 400 BAD REQUEST if appId is already used" $
    -- GIVEN: Prepare request
    do
      let reqDto = toChangeDTO (differentAppEdited {appId = "default"})
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

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "APP_PERM"
