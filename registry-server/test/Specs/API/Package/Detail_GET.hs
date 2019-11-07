module Specs.API.Package.Detail_GET
  ( detail_get
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
import Database.Migration.Development.Organization.Data.Organizations
import Database.Migration.Development.Package.Data.Packages
import LensesConfig
import Model.Context.AppContext
import Service.Package.PackageMapper

import Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /packages/{pkgId}
-- ------------------------------------------------------------------------
detail_get :: AppContext -> SpecWith Application
detail_get appContext =
  describe "GET /packages/{pkgId}" $ do
    test_200 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = BS.pack $ "/packages/" ++ (netherlandsPackageV2 ^. pId)

reqHeaders = [reqCtHeader]

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
    let expDto = toDetailDTO (toPackage netherlandsPackageV2) ["1.0.0", "2.0.0"] orgNetherlands
    let expBody = encode expDto
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest
    reqMethod
    "/packages/dsw.global:non-existing-package:1.0.0"
    reqHeaders
    reqBody
    "package"
    "dsw.global:non-existing-package:1.0.0"
