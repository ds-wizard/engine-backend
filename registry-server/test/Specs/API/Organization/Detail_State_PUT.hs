module Specs.API.Organization.Detail_State_PUT
  ( detail_state_put
  ) where

import Control.Lens ((&), (.~))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher

import Api.Resource.Organization.OrganizationDTO
import Api.Resource.Organization.OrganizationJM ()
import Api.Resource.Organization.OrganizationStateDTO
import Api.Resource.Organization.OrganizationStateJM ()
import Database.DAO.ActionKey.ActionKeyDAO
import Database.DAO.Organization.OrganizationDAO
import Database.Migration.Development.ActionKey.Data.ActionKeys
import Database.Migration.Development.Organization.Data.Organizations
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Error.ErrorHelpers
import Service.Organization.OrganizationMapper

import Specs.API.ActionKey.Common
import Specs.API.Common
import Specs.API.Organization.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- PUT /organizations/{orgId}/state
-- ------------------------------------------------------------------------
detail_state_put :: AppContext -> SpecWith Application
detail_state_put appContext =
  describe "PUT /organizations/{orgId}/state" $ do
    test_200 appContext
    test_400 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/organizations/dsw/state?hash=1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"

reqHeaders = [reqAdminAuthHeader]

reqDto = OrganizationStateDTO {_organizationStateDTOActive = True}

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
    let expDto = toDTO orgDsw
    let expBody = encode expDto
     -- AND: Prepare DB
    runInContextIO (insertActionKey regActionKey) appContext
    runInContextIO (updateOrganization (orgDsw & active .~ False)) appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, OrganizationDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareOrganizationDtos resBody expDto
     -- AND: Find result in DB and compare with expectation state
    assertExistenceOfOrganizationInDB appContext orgDsw

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = do
  createInvalidJsonTest reqMethod reqUrl [HJ.json| { } |] "active"
  it "HTTP 400 BAD REQUEST when hash is absent" $
     -- GIVEN: Prepare request
   do
    let reqUrl = "/organizations/dsw/state"
     -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = createErrorWithErrorMessage $ _ERROR_SERVICE_ORGANIZATION__REQUIRED_HASH_IN_QUERY_PARAMS
    let expBody = encode expDto
     -- AND: Prepare DB
    runInContextIO (insertActionKey regActionKey) appContext
    runInContextIO (updateOrganization (orgDsw & active .~ False)) appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertExistenceOfActionKeyInDB appContext regActionKey
    assertExistenceOfOrganizationInDB appContext (orgDsw & active .~ False)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext = do
  createNotFoundTest
    reqMethod
    "/organizations/nonexisting.dsw/state?hash=1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"
    reqHeaders
    reqBody
    "organization"
    "nonexisting.dsw"
  createNotFoundTest
    reqMethod
    "/organizations/dsw/state?hash=1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"
    reqHeaders
    reqBody
    "actionKey"
    "1ba90a0f-845e-41c7-9f1c-a55fc5a0554a"
