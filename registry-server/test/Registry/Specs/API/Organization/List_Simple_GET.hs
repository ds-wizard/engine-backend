module Registry.Specs.API.Organization.List_Simple_GET (
  list_simple_GET,
) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai

import Registry.Model.Context.AppContext
import Registry.Service.Organization.OrganizationMapper
import RegistryLib.Api.Resource.Organization.OrganizationSimpleJM ()
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import RegistryLib.Model.Organization.OrganizationSimple

import SharedTest.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /organizations/simple
-- ------------------------------------------------------------------------
list_simple_GET :: AppContext -> SpecWith ((), Application)
list_simple_GET appContext = describe "GET /organizations/simple" $ test_200 appContext

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
      let expType (a :: [OrganizationSimple]) = a
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      assertListResponse expStatus expHeaders expDto expType response
