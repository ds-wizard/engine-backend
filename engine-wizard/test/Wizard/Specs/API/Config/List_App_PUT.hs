module Wizard.Specs.API.Config.List_App_PUT
  ( list_app_PUT
  ) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.AppConfigMapper

import Wizard.Specs.API.Common
import Wizard.Specs.API.Config.Common

-- ------------------------------------------------------------------------
-- PUT /configs/app
-- ------------------------------------------------------------------------
list_app_PUT :: AppContext -> SpecWith ((), Application)
list_app_PUT appContext =
  describe "PUT /configs/app" $ do
    test_200 appContext
    test_400_invalid_json appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/configs/app"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = toChangeDTO editedAppConfig

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
    let expDto = editedAppConfig
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, AppConfig)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareDtos resBody expDto
     -- AND: Find result in DB and compare with expectation state
    assertExistenceOfAppConfigInDB appContext editedAppConfig

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_json appContext = createInvalidJsonTest reqMethod reqUrl "uuid"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "CFG_PERM"
