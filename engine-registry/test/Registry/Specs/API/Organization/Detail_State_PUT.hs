module Registry.Specs.API.Organization.Detail_State_PUT (
  detail_state_put,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationJM ()
import Registry.Api.Resource.Organization.OrganizationStateJM ()
import Registry.Database.DAO.ActionKey.ActionKeyDAO
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Database.Migration.Development.ActionKey.Data.ActionKeys
import Registry.Database.Migration.Development.Organization.Data.Organizations
import Registry.Model.Context.AppContext
import Registry.Model.Organization.Organization
import Registry.Service.Organization.OrganizationMapper
import Shared.Api.Resource.Error.ErrorJM ()

import Registry.Specs.API.Common
import Registry.Specs.API.Organization.Common
import Registry.Specs.Common
import SharedTest.Specs.API.Common

-- ------------------------------------------------------------------------
-- PUT /organizations/{orgId}/state
-- ------------------------------------------------------------------------
detail_state_put :: AppContext -> SpecWith ((), Application)
detail_state_put appContext =
  describe "PUT /organizations/{orgId}/state" $ do
    test_200 appContext
    test_400 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/organizations/global/state?hash=1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"

reqHeaders = [reqCtHeader]

reqDto = orgStateDto

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
      let expDto = toDTO orgGlobal
      let expType (a :: OrganizationDTO) = a
      -- AND: Prepare DB
      runInContextIO (insertActionKey regActionKey) appContext
      runInContextIO (updateOrganization (orgGlobal {active = False})) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertResponseWithoutFields expStatus expHeaders expDto expType response ["updatedAt"]
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfOrganizationInDB appContext orgGlobal

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod reqUrl "active"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest
    reqMethod
    "/organizations/global/state?hash=c996414a-b51d-4c8c-bc10-5ee3dab85fa8"
    reqHeaders
    reqBody
    "action_key"
    [("hash", "c996414a-b51d-4c8c-bc10-5ee3dab85fa8")]
