module Wizard.Specs.API.OpenIdClient.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Model.Context.AppContext
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeDTO
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientChangeJM ()
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailDTO
import WizardLib.Public.Api.Resource.OpenId.Client.Definition.OpenIdClientDetailJM ()
import WizardLib.Public.Database.DAO.OpenId.OpenIdClientDefinitionDAO (findOpenIdClientDefinitions)
import WizardLib.Public.Database.Migration.Development.OpenId.Data.OpenIdClients
import qualified WizardLib.Public.Database.Migration.Development.OpenId.OpenIdClientMigration as OPENID_Migration

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.OpenIdClient.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/open-id-clients
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /wizard-api/open-id-clients" $ do
    test_200 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/open-id-clients"

reqHeaders = [reqCtHeader, reqAuthHeader]

reqDto :: OpenIdClientChangeDTO
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
    assertCountInDB findOpenIdClientDefinitions appContext 2

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod reqUrl "name"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "SettingsManageRolePermission"
