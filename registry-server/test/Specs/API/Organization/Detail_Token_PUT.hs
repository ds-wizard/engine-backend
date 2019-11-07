module Specs.API.Organization.Detail_Token_PUT
  ( detail_token_put
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Api.Resource.Organization.OrganizationDTO
import Database.DAO.ActionKey.ActionKeyDAO
import Database.DAO.Organization.OrganizationDAO
import Database.Migration.Development.ActionKey.Data.ActionKeys
import Database.Migration.Development.Organization.Data.Organizations
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Error.ErrorHelpers

import Specs.API.ActionKey.Common
import Specs.API.Common
import Specs.Common

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

reqUrl = "/organizations/dsw/token?hash=5b1aff0d-b5e3-436d-b913-6b52d3cbad5f"

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
    let expHeaders = [resCtHeaderPlain] ++ resCorsHeadersPlain
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
    liftIO $ (orgFromDb ^. token /= orgDsw ^. token) `shouldBe` True
    liftIO $ (orgFromDb ^. token == resBody ^. token) `shouldBe` True

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = do
  it "HTTP 400 BAD REQUEST when hash is absent" $
     -- GIVEN: Prepare request
   do
    let reqUrl = "/organizations/dsw/token"
     -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = createErrorWithErrorMessage $ _ERROR_SERVICE_ORGANIZATION__REQUIRED_HASH_IN_QUERY_PARAMS
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
    "/organizations/nonexisting.dsw/token?hash=5b1aff0d-b5e3-436d-b913-6b52d3cbad5f"
    reqHeaders
    reqBody
    "organization"
    "nonexisting.dsw"
  createNotFoundTest
    reqMethod
    "/organizations/dsw/token?hash=5b1aff0d-b5e3-436d-b913-6b52d3cbad5f"
    reqHeaders
    reqBody
    "actionKey"
    "5b1aff0d-b5e3-436d-b913-6b52d3cbad5f"
