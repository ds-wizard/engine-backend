module Specs.API.Organization.Detail_PUT
  ( detail_put
  ) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher

import Api.Resource.Organization.OrganizationDTO
import Database.DAO.Organization.OrganizationDAO
import Database.Migration.Development.Organization.Data.Organizations
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Error.ErrorHelpers
import Service.Organization.OrganizationMapper

import Specs.API.Common
import Specs.API.Organization.Common

-- ------------------------------------------------------------------------
-- PUT /organizations/{orgId}
-- ------------------------------------------------------------------------
detail_put :: AppContext -> SpecWith Application
detail_put appContext =
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

reqUrl = "/organizations/dsw"

reqHeaders = [reqAdminAuthHeader]

reqDto = editedOrgDswChange

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
    let expDto = toDTO editedOrgDsw
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, OrganizationDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareOrganizationDtos resBody expDto
     -- AND: Find result in DB and compare with expectation state
    assertExistenceOfOrganizationInDB appContext editedOrgDsw

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = do
  createInvalidJsonTest reqMethod reqUrl [HJ.json| { name: "Common KM" } |] "description"
  it "HTTP 400 BAD REQUEST when email is already used" $
     -- GIVEN: Prepare request
   do
    let orgEmail = orgNetherlands ^. email
    let reqDto = orgDswCreate & email .~ orgEmail
    let reqBody = encode reqDto
     -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = createErrorWithFieldError ("email", _ERROR_VALIDATION__ENTITY_UNIQUENESS "Email" orgEmail)
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
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createForbiddenTest reqMethod reqUrl [reqUserAuthHeader] reqBody "Detail Organization"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest reqMethod "/organizations/nonexisting.dsw" reqHeaders reqBody "organization" "nonexisting.dsw"
