module Registry.Specs.API.Locale.Detail_Bundle_GET (
  detail_bundle_GET,
) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import qualified Registry.Database.Migration.Development.Locale.LocaleMigration as LOC_Migration
import Registry.Model.Context.AppContext

import Registry.Specs.API.Common
import Registry.Specs.Common
import SharedTest.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /locales/{lclId}/bundle
-- ------------------------------------------------------------------------
detail_bundle_GET :: AppContext -> SpecWith ((), Application)
detail_bundle_GET appContext =
  describe "GET /locales/{lclId}/bundle" $ do
    test_200 appContext
    test_401 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/locales/global:dutch:1.0.0/bundle"

reqHeaders = [reqAdminAuthHeader, reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCorsHeadersPlain
      -- AND: Run migrations
      runInContextIO LOC_Migration.runMigration appContext
      runInContextIO LOC_Migration.runS3Migration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, String)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/locales/global:non-existing-locale:1.0.0/bundle"
    reqHeaders
    reqBody
    "locale"
    [("organization_id", "global"), ("locale_id", "non-existing-locale"), ("version", "1.0.0")]
