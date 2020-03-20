module Wizard.Specs.API.Config.List_Application_PUT
  ( list_application_PUT
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ

import LensesConfig
import Wizard.Api.Resource.Config.AppConfigDTO
import Wizard.Api.Resource.Config.AppConfigJM ()
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Model.Context.AppContext
import Wizard.Service.Config.AppConfigMapper

import Wizard.Specs.API.Common
import Wizard.Specs.API.Config.Common

-- ------------------------------------------------------------------------
-- PUT /configs/application
-- ------------------------------------------------------------------------
list_application_PUT :: AppContext -> SpecWith Application
list_application_PUT appContext =
  describe "PUT /configs/application" $ do
    test_200 appContext
    test_400_invalid_json appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/configs/application"

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
    let expHeaders = [resCtHeaderPlain] ++ resCorsHeadersPlain
    let expDto = toDTO editedAppConfig
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, AppConfigDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareAppConfigDtos resBody expDto
     -- AND: Find result in DB and compare with expectation state
    assertExistenceOfAppConfigInDB appContext editedAppConfig

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_json appContext = createInvalidJsonTest reqMethod reqUrl [HJ.json| { name: "Common KM" } |] "uuid"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext =
  createNoPermissionTest (appContext ^. applicationConfig) reqMethod reqUrl [reqCtHeader] reqBody "CFG_PERM"
