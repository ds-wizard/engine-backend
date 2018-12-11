module Specs.API.Package.List_POST
  ( list_post
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher

import Api.Resource.Error.ErrorDTO ()
import Api.Resource.Package.PackageWithEventsDTO ()
import Database.DAO.Package.PackageDAO
import Database.Migration.Development.Package.Data.Packages
import LensesConfig
import Model.Context.AppContext
import Service.Package.PackageMapper

import Specs.API.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /packages
-- ------------------------------------------------------------------------
list_post :: AppContext -> SpecWith Application
list_post appContext =
  describe "POST /packages" $ do
    test_201 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/packages"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = packageWithEventsToDTOWithEvents baseElixirPackageDto

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext = do
  it "HTTP 201 CREATED" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 201
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = packageWithEventsToDTO baseElixirPackageDto
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO deletePackages appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod reqUrl [HJ.json| { name: "Common Package" } |] "id"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest (appContext ^. config) reqMethod reqUrl [] "" "PM_WRITE_PERM"
