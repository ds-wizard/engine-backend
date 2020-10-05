module Wizard.Specs.API.Package.List_Page_GET
  ( list_page_get
  ) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.Migration.Development.Organization.Data.Organizations
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Service.Package.PackageMapper
import Wizard.Database.Migration.Development.Package.Data.Packages
import qualified Wizard.Database.Migration.Development.Package.PackageMigration as PKG
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import Wizard.Model.Context.AppContext
import Wizard.Service.Package.PackageMapper

import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /packages/page
-- ------------------------------------------------------------------------
list_page_get :: AppContext -> SpecWith ((), Application)
list_page_get appContext =
  describe "GET /packages/page" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/packages/page"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  let expOrgRs = [orgGlobalSimple, orgNetherlandsSimple]
  create_test_200
    "HTTP 200 OK"
    appContext
    "/packages/page?sort=kmId,asc"
    (Page
       "packages"
       (PageMetadata 20 3 1 0)
       [ toSimpleDTO' [globalRemotePackage] expOrgRs ["0.0.1", "1.0.0"] (toPackage globalPackage)
       , toSimpleDTO' [] expOrgRs ["1.0.0"] (toPackage germanyPackage)
       , toSimpleDTO' [globalNetherlandsPackage] expOrgRs ["1.0.0", "2.0.0"] (toPackage netherlandsPackageV2)
       ])
  create_test_200
    "HTTP 200 OK (query - q)"
    appContext
    "/packages/page?q=Germany Knowledge Model"
    (Page "packages" (PageMetadata 20 1 1 0) [toSimpleDTO' [] expOrgRs ["1.0.0"] (toPackage germanyPackage)])
  create_test_200
    "HTTP 200 OK (query - kmId)"
    appContext
    "/packages/page?kmId=core-nl"
    (Page
       "packages"
       (PageMetadata 20 1 1 0)
       [toSimpleDTO' [globalNetherlandsPackage] expOrgRs ["1.0.0", "2.0.0"] (toPackage netherlandsPackageV2)])
  create_test_200
    "HTTP 200 OK (query for non-existing)"
    appContext
    "/packages/page?q=Non-existing Knowledge Model"
    (Page "packages" (PageMetadata 20 0 0 0) [])

create_test_200 title appContext reqUrl expDto =
  it title $
       -- GIVEN: Prepare request
   do
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO PKG.runMigration appContext
    runInContextIO QTN.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "PM_READ_PERM"
