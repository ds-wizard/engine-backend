module Wizard.Specs.API.OpenIdClient.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Model.Context.AppContext
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeJM ()
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailDTO
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailJM ()
import WizardLib.Public.Database.Migration.Development.OpenId.Data.OpenIdClients
import qualified WizardLib.Public.Database.Migration.Development.OpenId.OpenIdClientMigration as OPENID_Migration

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.OpenIdClient.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/open-id-clients/{uuid}
-- ------------------------------------------------------------------------
detail_PUT :: AppContext -> SpecWith ((), Application)
detail_PUT appContext =
  describe "PUT /wizard-api/open-id-clients/{uuid}" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/open-id-clients/cb7558d8-5e78-4494-9b94-0e9d64676923"

reqHeaders = [reqCtHeader, reqAuthHeader]

reqDto = defaultOpenIdClientChangeDto

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    let expDto = editedOpenIdClientDetailDto
    -- AND: Run migrations
    runInContextIO OPENID_Migration.runMigration appContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, OpenIdClientDetailDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareOpenIdClientDetailDtos resDto expDto
    -- AND: Find result in DB and compare with expectation state
    assertExistenceOfOpenIdClientInDB appContext editedOpenIdClient

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "SettingsManageRolePermission"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/open-id-clients/99193032-99e3-4676-acd8-222983ea0b88"
    reqHeaders
    reqBody
    "openid_client"
    [("uuid", "99193032-99e3-4676-acd8-222983ea0b88")]
