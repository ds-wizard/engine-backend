module Specs.API.Package.Detail_Bundle_GET
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

import Api.Resource.Error.ErrorDTO ()
import Api.Resource.Package.PackageDetailJM ()
import Database.Migration.Development.Audit.Data.AuditEntries
import Database.Migration.Development.Package.Data.Packages
import Database.Migration.Development.PackageBundle.Data.PackageBundles
import LensesConfig
import Model.Context.AppContext
import Service.PackageBundle.PackageBundleMapper

import Specs.API.Audit.Common
import Specs.API.Common

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
test_200 appContext = do
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
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest
    reqMethod
    "/packages/dsw.global:non-existing-package:1.0.0/bundle"
    reqHeaders
    reqBody
    "package"
    "dsw.global:non-existing-package:1.0.0"
