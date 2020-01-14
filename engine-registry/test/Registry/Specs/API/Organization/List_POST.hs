module Registry.Specs.API.Organization.List_POST
  ( list_post
  ) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher

import LensesConfig
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Database.Migration.Development.Organization.Data.Organizations
import Registry.Localization.Messages.Public
import Registry.Model.Context.AppContext
import Registry.Service.Organization.OrganizationMapper

import Registry.Specs.API.Common
import Registry.Specs.API.Organization.Common
import Registry.Specs.Common
import SharedTest.Specs.Common

-- ------------------------------------------------------------------------
-- POST /organizations
-- ------------------------------------------------------------------------
list_post :: AppContext -> SpecWith Application
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

reqHeaders = []

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
    let expHeaders = [resCtHeaderPlain] ++ resCorsHeadersPlain
    let expDto = toDTO (orgGlobal & active .~ False)
     -- AND: Prepare DB
    runInContextIO deleteOrganizations appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, OrganizationDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareOrganizationDtosWhenCreate resBody expDto
     -- AND: Find result in DB and compare with expectation state
    organizationFromDb <- getFirstFromDB findOrganizations appContext
    compareOrganizationDtosWhenCreate organizationFromDb reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_json appContext =
  createInvalidJsonTest reqMethod reqUrl [HJ.json| { name: "Common KM" } |] "organizationId"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_organizationId appContext =
  it "HTTP 400 BAD REQUEST when organizationId is not in valid format" $
     -- GIVEN: Prepare request
   do
    let reqDto = orgGlobalCreate & organizationId .~ "organization-amsterdam"
    let reqBody = encode reqDto
     -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = createValidationError [] [("organizationId", _ERROR_VALIDATION__INVALID_ORGANIZATION_ID_FORMAT)]
    let expBody = encode expDto
     -- AND: Prepare DB
    runInContextIO deleteOrganizations appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertCountInDB findOrganizations appContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_organizationId_duplication appContext =
  it "HTTP 400 BAD REQUEST when organizationId is already used" $
     -- GIVEN: Prepare request
   do
    let orgId = orgGlobalCreate ^. organizationId
     -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto =
          createValidationError [] [("organizationId", _ERROR_VALIDATION__ENTITY_UNIQUENESS "Organization" orgId)]
    let expBody = encode expDto
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertCountInDB findOrganizations appContext 2

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_email_duplication appContext =
  it "HTTP 400 BAD REQUEST when email is already used" $
     -- GIVEN: Prepare request
   do
    let orgEmail = orgGlobalCreate ^. email
    let reqDto = orgGlobalCreate & organizationId .~ "org.de"
    let reqBody = encode reqDto
     -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = createValidationError [] [("email", _ERROR_VALIDATION__ENTITY_UNIQUENESS "Email" orgEmail)]
    let expBody = encode expDto
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertCountInDB findOrganizations appContext 2
