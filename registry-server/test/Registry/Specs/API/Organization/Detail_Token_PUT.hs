module Registry.Specs.API.Organization.Detail_Token_PUT (
  detail_token_PUT,
) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

import Registry.Database.DAO.Organization.OrganizationDAO
import Registry.Database.Migration.Development.ActionKey.Data.ActionKeys
import Registry.Model.Context.AppContext
import Registry.Service.Organization.OrganizationMapper
import RegistryLib.Api.Resource.Organization.OrganizationDTO
import RegistryLib.Api.Resource.Organization.OrganizationJM ()
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import RegistryLib.Model.Organization.Organization
import Shared.ActionKey.Database.DAO.ActionKey.ActionKeyDAO

import Registry.Specs.API.Common
import Registry.Specs.Common
import SharedTest.Specs.API.Common

-- ------------------------------------------------------------------------
-- PUT /organizations/{orgId}/token
-- ------------------------------------------------------------------------
detail_token_PUT :: AppContext -> SpecWith ((), Application)
detail_token_PUT appContext =
  describe "PUT /organizations/{orgId}/token" $ do
    test_200 appContext
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
      let expDto = toDTO orgGlobal
      let expType (a :: OrganizationDTO) = a
      -- AND: Prepare DB
      runInContextIO (insertActionKey forgottenTokenActionKey) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertResponseWithoutFields expStatus expHeaders expDto expType response ["token", "updatedAt"]
      -- AND: Find result in DB and compare with expectation state
      orgFromDb <- getFirstFromDB findOrganizations appContext
      liftIO $ (orgFromDb.token /= orgGlobal.token) `shouldBe` True

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/organizations/global/token?hash=c996414a-b51d-4c8c-bc10-5ee3dab85fa8"
    reqHeaders
    reqBody
    "action_key"
    [("hash", "c996414a-b51d-4c8c-bc10-5ee3dab85fa8")]
