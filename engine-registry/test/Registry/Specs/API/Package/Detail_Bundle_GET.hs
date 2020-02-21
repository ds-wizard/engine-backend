module Registry.Specs.API.Package.Detail_Bundle_GET
  ( detail_bundle_get
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig
import Registry.Api.Resource.Package.PackageDetailJM ()
import Registry.Api.Resource.PackageBundle.PackageBundleJM ()
import Registry.Database.Migration.Development.Audit.Data.AuditEntries
import Registry.Database.Migration.Development.Package.Data.Packages
import Registry.Database.Migration.Development.PackageBundle.Data.PackageBundles
import Registry.Model.Context.AppContext
import Registry.Service.PackageBundle.PackageBundleMapper
import Shared.Api.Resource.Error.ErrorDTO ()

import Registry.Specs.API.Audit.Common
import Registry.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /packages/{pkgId}/bundle
-- ------------------------------------------------------------------------
detail_bundle_get :: AppContext -> SpecWith Application
detail_bundle_get appContext =
  describe "GET /packages/{pkgId}/bundle" $ do
    test_200 appContext
    test_401 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = BS.pack $ "/packages/" ++ (netherlandsPackageV2 ^. pId) ++ "/bundle"

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
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = toDTO netherlandsPackageV2Budle
    let expBody = encode expDto
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertExistenceOfAuditEntryInDB appContext getPackageBundleAuditEntry

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest
    reqMethod
    "/packages/global:non-existing-package:1.0.0/bundle"
    reqHeaders
    reqBody
    "package"
    "global:non-existing-package:1.0.0"
