module Registry.Specs.API.Organization.List_POST (
  list_post,
) where

import Data.Aeson (encode)
import qualified Data.Map.Strict as M
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Localization.Messages.Public
import Registry.Model.Context.AppContext
import Registry.Service.Organization.OrganizationMapper
import RegistryLib.Api.Resource.Organization.OrganizationCreateDTO
import RegistryLib.Api.Resource.Organization.OrganizationCreateJM ()
import RegistryLib.Api.Resource.Organization.OrganizationDTO
import RegistryLib.Api.Resource.Organization.OrganizationJM ()
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import RegistryLib.Model.Organization.Organization
import RegistryLib.Model.Organization.OrganizationRole
import Shared.Common.Model.Error.Error

import Registry.Specs.API.Common
import Registry.Specs.API.Organization.Common
import Registry.Specs.Common
import SharedTest.Specs.API.Common

-- ------------------------------------------------------------------------
-- POST /organizations
-- ------------------------------------------------------------------------
list_post :: AppContext -> SpecWith ((), Application)
list_post appContext =
  describe "POST /organizations" $ do
    test_201 appContext
    test_400_invalid_json appContext
    test_400_invalid_organizationId appContext
    test_400_organizationId_duplication appContext
    test_400_email_duplication appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/organizations"

reqHeaders = [reqCtHeader]

reqDto = orgGlobalCreate

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext =
  it "HTTP 201 CREATED" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = toDTO (orgGlobal {active = False, logo = Nothing, oRole = UserRole})
      let expType (a :: OrganizationDTO) = a
      -- AND: Prepare DB
      runInContextIO deleteOrganizations appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertResponseWithoutFields expStatus expHeaders expDto expType response ["token", "createdAt", "updatedAt"]
      -- AND: Find result in DB and compare with expectation state
      organizationFromDb <- getFirstFromDB findOrganizations appContext
      compareOrganizationDtosWhenCreate organizationFromDb reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_json appContext = createInvalidJsonTest reqMethod reqUrl "organizationId"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_organizationId appContext =
  it "HTTP 400 BAD REQUEST when organizationId is not in valid format" $
    -- GIVEN: Prepare request
    do
      let reqDto = orgGlobalCreate {organizationId = "organization-amsterdam"} :: OrganizationCreateDTO
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = ValidationError [] (M.singleton "organizationId" [_ERROR_VALIDATION__INVALID_ORGANIZATION_ID_FORMAT])
      let expType (a :: AppError) = a
      -- AND: Prepare DB
      runInContextIO deleteOrganizations appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertResponse expStatus expHeaders expDto expType response
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findOrganizations appContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_organizationId_duplication appContext =
  it "HTTP 400 BAD REQUEST when organizationId is already used" $
    -- GIVEN: Prepare request
    do
      let orgId = orgGlobalCreate.organizationId
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = ValidationError [] (M.singleton "organizationId" [_ERROR_VALIDATION__ORGANIZATION_ID_UNIQUENESS orgId])
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
test_400_email_duplication appContext =
  it "HTTP 400 BAD REQUEST when email is already used" $
    -- GIVEN: Prepare request
    do
      let orgEmail = orgGlobalCreate.email
      let reqDto = orgGlobalCreate {organizationId = "org.de"} :: OrganizationCreateDTO
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
