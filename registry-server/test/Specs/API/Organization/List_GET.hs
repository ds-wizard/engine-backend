module Specs.API.Organization.List_GET
  ( list_get
  ) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Api.Resource.Organization.OrganizationDTO
import Api.Resource.Organization.OrganizationJM ()
import Database.Migration.Development.Organization.Data.Organizations
import Model.Context.AppContext
import Service.Organization.OrganizationMapper

import Specs.API.Common
import Specs.API.Organization.Common

-- ------------------------------------------------------------------------
-- GET /organizations
-- ------------------------------------------------------------------------
list_get :: AppContext -> SpecWith Application
list_get appContext =
  describe "GET /organizations" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/organizations"

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
    let expDto = toDTO <$> [orgDsw, orgNetherlands]
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, [OrganizationDTO])
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareOrganizationDtos (resBody !! 0) (expDto !! 0)
    compareOrganizationDtos (resBody !! 1) (expDto !! 1)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createForbiddenTest reqMethod reqUrl [reqUserAuthHeader] reqBody "List Organizations"
