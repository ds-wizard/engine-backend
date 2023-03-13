module Wizard.Specs.API.Package.List_Suggestions_GET (
  list_suggestions_GET,
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
import Wizard.Api.Resource.Package.PackageSuggestionJM ()
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import qualified Wizard.Database.Migration.Development.Package.PackageMigration as PKG
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Model.Package.PackageSuggestion
import Wizard.Service.Package.PackageMapper

import SharedTest.Specs.API.Common
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
    "/packages/suggestions?sort=organizationId,asc"
    ( Page
        "packages"
        (PageMetadata 20 3 1 0)
        [ toSuggestion (toPackage globalPackage, ["0.0.1", "1.0.0"])
        , toSuggestion (toPackage germanyPackage, ["1.0.0"])
        , toSuggestion (toPackage netherlandsPackageV2, ["1.0.0", "2.0.0"])
        ]
    )
  create_test_200
    "HTTP 200 OK (select)"
    appContext
    "/packages/suggestions?sort=organizationId,asc&select=org.de:core-de:all,org.nl:core-nl:all"
    ( Page
        "packages"
        (PageMetadata 20 2 1 0)
        [ toSuggestion (toPackage germanyPackage, ["1.0.0"])
        , toSuggestion (toPackage netherlandsPackageV2, ["1.0.0", "2.0.0"])
        ]
    )
  create_test_200
    "HTTP 200 OK (exclude)"
    appContext
    "/packages/suggestions?sort=organizationId,asc&exclude=org.de:core-de:all,org.nl:core-nl:all"
    (Page "packages" (PageMetadata 20 1 1 0) [toSuggestion (toPackage globalPackage, ["0.0.1", "1.0.0"])])
  create_test_200
    "HTTP 200 OK (query - q)"
    appContext
    "/packages/suggestions?q=Germany Knowledge Model"
    (Page "packages" (PageMetadata 20 1 1 0) [toSuggestion (toPackage germanyPackage, ["1.0.0"])])
  create_test_200
    "HTTP 200 OK (phase)"
    appContext
    "/packages/suggestions?sort=organizationId,asc&phase=ReleasedPackagePhase"
    ( Page
        "packages"
        (PageMetadata 20 3 1 0)
        [ toSuggestion (toPackage globalPackage, ["0.0.1", "1.0.0"])
        , toSuggestion (toPackage germanyPackage, ["1.0.0"])
        , toSuggestion (toPackage netherlandsPackageV2, ["1.0.0", "2.0.0"])
        ]
    )
  create_test_200
    "HTTP 200 OK (query for non-existing)"
    appContext
    "/packages/suggestions?q=Non-existing Knowledge Model"
    (Page "packages" (PageMetadata 20 0 0 0) ([] :: [PackageSuggestion]))

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
      runInContextIO TML.runMigration appContext
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
