module Wizard.Specs.API.Package.List_Suggestions_GET
  ( list_suggestions_GET
  ) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Model.Common.Page
import Shared.Model.Common.PageMetadata
import Shared.Service.Package.PackageMapper
import qualified Wizard.Database.Migration.Development.Package.PackageMigration as PKG
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext

import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /packages/suggestions
-- ------------------------------------------------------------------------
list_suggestions_GET :: AppContext -> SpecWith ((), Application)
list_suggestions_GET appContext =
  describe "GET /packages/suggestions" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/packages/suggestions"

reqHeaders = [reqNonAdminAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK"
    appContext
    "/packages/suggestions?sort=versions.name,asc"
    (Page
       "packages"
       (PageMetadata 20 3 1 0)
       [ toSuggestionDTO germanyPackageGroup
       , toSuggestionDTO globalPackageGroup
       , toSuggestionDTO netherlandsPackageGroup
       ])
  create_test_200
    "HTTP 200 OK (query - q)"
    appContext
    "/packages/suggestions?q=Germany Knowledge Model"
    (Page "packages" (PageMetadata 20 1 1 0) [toSuggestionDTO germanyPackageGroup])
  create_test_200
    "HTTP 200 OK (query for non-existing)"
    appContext
    "/packages/suggestions?q=Non-existing Knowledge Model"
    (Page "packages" (PageMetadata 20 0 0 0) [])

create_test_200 title appContext reqUrl expDto =
  it title $
       -- GIVEN: Prepare request
   do
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO U.runMigration appContext
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
