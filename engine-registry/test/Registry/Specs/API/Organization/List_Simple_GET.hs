module Registry.Specs.API.Organization.List_Simple_GET
  ( list_simple_get
  ) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Registry.Database.Migration.Development.Organization.Data.Organizations
import Registry.Model.Context.AppContext
import Registry.Service.Organization.OrganizationMapper
import Shared.Api.Resource.Organization.OrganizationSimpleDTO
import Shared.Api.Resource.Organization.OrganizationSimpleJM ()

import Registry.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /organizations/simple
-- ------------------------------------------------------------------------
list_simple_get :: AppContext -> SpecWith Application
list_simple_get appContext = describe "GET /organizations/simple" $ test_200 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/organizations/simple"

reqHeaders = []

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
    let expDto = toSimpleDTO <$> [orgGlobal, orgNetherlands]
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, [OrganizationSimpleDTO])
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    liftIO $ head resBody `shouldBe` head expDto
    liftIO $ (resBody !! 1) `shouldBe` (expDto !! 1)
