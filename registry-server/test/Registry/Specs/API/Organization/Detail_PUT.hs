module Registry.Specs.API.Organization.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

import Registry.Api.Resource.Organization.OrganizationChangeJM ()
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Database.Migration.Development.Organization.Data.Organizations
import Registry.Localization.Messages.Public
import Registry.Model.Context.AppContext
import Registry.Service.Organization.OrganizationMapper
import RegistryLib.Api.Resource.Organization.OrganizationCreateDTO
import RegistryLib.Api.Resource.Organization.OrganizationCreateJM ()
import RegistryLib.Api.Resource.Organization.OrganizationDTO
import RegistryLib.Api.Resource.Organization.OrganizationJM ()
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import RegistryLib.Model.Organization.Organization
import Shared.Common.Model.Error.Error

import Registry.Specs.API.Common
import Registry.Specs.API.Organization.Common
import SharedTest.Specs.API.Common

-- ------------------------------------------------------------------------
-- PUT /organizations/{orgId}
-- ------------------------------------------------------------------------
detail_PUT :: AppContext -> SpecWith ((), Application)
detail_PUT appContext =
  describe "PUT /organizations/{orgId}" $ do
    test_200 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/organizations/global"

reqHeaders = [reqCtHeader, reqAdminAuthHeader]

reqDto = orgGlobalEditedChange

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
      let expDto = toDTO orgGlobalEdited
      let expType (a :: OrganizationDTO) = a
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertResponseWithoutFields expStatus expHeaders expDto expType response ["updatedAt"]
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfOrganizationInDB appContext orgGlobalEdited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = do
  createInvalidJsonTest reqMethod reqUrl "description"
  it "HTTP 400 BAD REQUEST when email is already used" $
    -- GIVEN: Prepare request
    do
      let orgEmail = orgNetherlands.email
      let reqDto = orgGlobalCreate {email = orgEmail} :: OrganizationCreateDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = ValidationError [] (M.singleton "email" [_ERROR_VALIDATION__ORGANIZATION_EMAIL_UNIQUENESS orgEmail])
      let expType (a :: AppError) = a
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertResponse expStatus expHeaders expDto expType response
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findOrganizations appContext 2

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext =
  createForbiddenTest reqMethod reqUrl [reqCtHeader, reqUserAuthHeader] reqBody "Detail Organization"

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
