module Registry.Specs.API.Organization.Detail_Token_PUT
  ( detail_token_put
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Api.Resource.Organization.OrganizationJM ()
import Registry.Database.DAO.ActionKey.ActionKeyDAO
import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Database.Migration.Development.ActionKey.Data.ActionKeys
import Registry.Database.Migration.Development.Organization.Data.Organizations
import Registry.Model.Context.AppContext
import Registry.Service.Organization.OrganizationMapper
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error

import Registry.Specs.API.ActionKey.Common
import Registry.Specs.API.Common
import Registry.Specs.API.Organization.Common
import Registry.Specs.Common
import SharedTest.Specs.API.Common

-- ------------------------------------------------------------------------
-- PUT /organizations/{orgId}/token
-- ------------------------------------------------------------------------
detail_token_put :: AppContext -> SpecWith ((), Application)
detail_token_put appContext =
  describe "PUT /organizations/{orgId}/token" $ do
    test_200 appContext
    test_400 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/organizations/global/token?hash=5b1aff0d-b5e3-436d-b913-6b52d3cbad5f"

reqHeaders = [reqCtHeader]

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
     -- AND: Prepare DB
    runInContextIO (insertActionKey forgTokActionKey) appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    assertResponse' expStatus expHeaders expDto expType response []
     -- AND: Find result in DB and compare with expectation state
    orgFromDb <- getFirstFromDB findOrganizations appContext
    liftIO $ (orgFromDb ^. token /= orgGlobal ^. token) `shouldBe` True

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = do
  it "HTTP 400 BAD REQUEST when hash is absent" $
     -- GIVEN: Prepare request
   do
    let reqUrl = "/organizations/global/token"
     -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = UserError _ERROR_VALIDATION__HASH_ABSENCE
    let expBody = encode expDto
     -- AND: Prepare DB
    runInContextIO (insertActionKey forgTokActionKey) appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertExistenceOfActionKeyInDB appContext forgTokActionKey
  it "HTTP 400 BAD REQUEST when hash is not in DB" $
     -- GIVEN: Prepare request
   do
    let reqUrl = "/organizations/global/token?hash=c996414a-b51d-4c8c-bc10-5ee3dab85fa8"
     -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = UserError _ERROR_VALIDATION__HASH_ABSENCE
    let expBody = encode expDto
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertExistenceOfOrganizationInDB appContext orgGlobal
