module Registry.Specs.API.Organization.Detail_State_PUT
  ( detail_state_put
  ) where

import Control.Lens ((&), (.~))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationJM ()
import Registry.Api.Resource.Organization.OrganizationStateJM ()
import Registry.Database.DAO.ActionKey.ActionKeySqlDAO
import Registry.Database.DAO.Organization.OrganizationSqlDAO
import Registry.Database.Migration.Development.ActionKey.Data.ActionKeys
import Registry.Database.Migration.Development.Organization.Data.Organizations
import Registry.Model.Context.AppContext
import Registry.Service.Organization.OrganizationMapper
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error

import Registry.Specs.API.ActionKey.Common
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
    runInContextIO (updateOrganization (orgGlobal & active .~ False)) appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    assertResponse expStatus expHeaders expDto expType response ["updatedAt"]
     -- AND: Find result in DB and compare with expectation state
    assertExistenceOfOrganizationInDB appContext orgGlobal

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = do
  createInvalidJsonTest reqMethod reqUrl "active"
  it "HTTP 400 BAD REQUEST when hash is absent" $
     -- GIVEN: Prepare request
   do
    let reqUrl = "/organizations/global/state"
     -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = UserError _ERROR_VALIDATION__HASH_ABSENCE
    let expBody = encode expDto
     -- AND: Prepare DB
    runInContextIO (insertActionKey regActionKey) appContext
    runInContextIO (updateOrganization (orgGlobal & active .~ False)) appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertExistenceOfActionKeyInDB appContext regActionKey
    assertExistenceOfOrganizationInDB appContext (orgGlobal & active .~ False)
  it "HTTP 400 BAD REQUEST when hash is not in DB" $
     -- GIVEN: Prepare request
   do
    let reqUrl = "/organizations/global/state?hash=c996414a-b51d-4c8c-bc10-5ee3dab85fa8"
     -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = UserError _ERROR_VALIDATION__HASH_ABSENCE
    let expBody = encode expDto
     -- AND: Prepare DB
    runInContextIO (updateOrganization (orgGlobal & active .~ False)) appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertExistenceOfOrganizationInDB appContext (orgGlobal & active .~ False)
