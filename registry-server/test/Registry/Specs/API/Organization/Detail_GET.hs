module Registry.Specs.API.Organization.Detail_GET (
  detail_GET,
) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

import Registry.Model.Context.AppContext
import Registry.Service.Organization.OrganizationMapper
import RegistryLib.Api.Resource.Organization.OrganizationDTO
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations

import Registry.Specs.API.Common
import SharedTest.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /organizations/{orgId}
-- ------------------------------------------------------------------------
detail_GET :: AppContext -> SpecWith ((), Application)
detail_GET appContext =
  describe "GET /organizations/{orgId}" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/organizations/global"

reqHeaders = [reqAdminAuthHeader]

reqBody = ""

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
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertResponse expStatus expHeaders expDto expType response

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createForbiddenTest reqMethod reqUrl [reqUserAuthHeader] reqBody "Detail Organization"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest
    reqMethod
    "/organizations/nonexisting.organization"
    reqHeaders
    reqBody
    "organization"
    [("organization_id", "nonexisting.organization")]
