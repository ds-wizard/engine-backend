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
import Registry.Localization.Messages.Public
import Registry.Model.Context.AppContext

import Registry.Specs.API.ActionKey.Common
import Registry.Specs.API.Common
import Registry.Specs.Common
import SharedTest.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /organizations/{orgId}/token
-- ------------------------------------------------------------------------
detail_token_put :: AppContext -> SpecWith Application
detail_token_put appContext =
  describe "PUT /organizations/{orgId}/token" $ do
    test_200 appContext
    test_400 appContext
    test_404 appContext

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
     -- AND: Prepare DB
    runInContextIO (insertActionKey forgTokActionKey) appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, OrganizationDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
     -- AND: Find result in DB and compare with expectation state
    orgFromDb <- getFirstFromDB findOrganizations appContext
    liftIO $ (orgFromDb ^. token /= orgGlobal ^. token) `shouldBe` True
    liftIO $ (orgFromDb ^. token == resBody ^. token) `shouldBe` True

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext =
  it "HTTP 400 BAD REQUEST when hash is absent" $
     -- GIVEN: Prepare request
   do
    let reqUrl = "/organizations/global/token"
     -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = createUserError _ERROR_SERVICE_ORGANIZATION__REQUIRED_HASH_IN_QUERY_PARAMS
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

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext = do
  createNotFoundTest
    reqMethod
    "/organizations/nonexisting.organization/token?hash=5b1aff0d-b5e3-436d-b913-6b52d3cbad5f"
    reqHeaders
    reqBody
    "organization"
    "nonexisting.organization"
  createNotFoundTest
    reqMethod
    "/organizations/global/token?hash=5b1aff0d-b5e3-436d-b913-6b52d3cbad5f"
    reqHeaders
    reqBody
    "actionKey"
    "5b1aff0d-b5e3-436d-b913-6b52d3cbad5f"
