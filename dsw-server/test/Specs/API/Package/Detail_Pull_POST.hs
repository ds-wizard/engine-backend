module Specs.API.Package.Detail_Pull_POST
  ( detail_pull_post
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Api.Resource.Error.ErrorJM ()
import Database.DAO.Package.PackageDAO
import Database.Migration.Development.Package.Data.Packages
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Error.Error

import Specs.API.Common
import Specs.API.Package.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /packages/{pkgId}
-- ------------------------------------------------------------------------
detail_pull_post :: AppContext -> SpecWith Application
detail_pull_post appContext =
  describe "POST /packages/{pkgId}/pull" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = BS.pack $ "/packages/" ++ (globalPackage ^. pId) ++ "/pull"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  it "HTTP 200 OK" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 204
    let expHeaders = resCorsHeaders
    let expBody = ""
     -- AND: Run migrations
    runInContextIO deletePackages appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertCountInDB findPackages appContext 1
    assertExistenceOfPackageInDB appContext globalPackage

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest (appContext ^. appConfig) reqMethod reqUrl [] "" "PM_WRITE_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  it "HTTP 404 NOT FOUND - Package was not found in Registry" $
      -- GIVEN: Prepare request
   do
    let reqUrl = "/packages/dsw.global:non-existing-package:1.0.0/pull"
     -- AND: Prepare expectation
    let expStatus = 404
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = NotExistsError (_ERROR_SERVICE_PB__PULL_NON_EXISTING_PKG "dsw.global:non-existing-package:1.0.0")
    let expBody = encode expDto
      -- WHEN: Call APIA
    response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
