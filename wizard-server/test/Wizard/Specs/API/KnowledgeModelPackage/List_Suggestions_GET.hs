module Wizard.Specs.API.KnowledgeModelPackage.List_Suggestions_GET (
  list_suggestions_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSuggestionJM ()
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.Package.KnowledgeModelPackageSuggestion
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/knowledge-model-packages/suggestions
-- ------------------------------------------------------------------------
list_suggestions_GET :: AppContext -> SpecWith ((), Application)
list_suggestions_GET appContext =
  describe "GET /wizard-api/knowledge-model-packages/suggestions" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/knowledge-model-packages/suggestions"

reqHeaders = [reqNonAdminAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK"
    appContext
    "/wizard-api/knowledge-model-packages/suggestions?sort=organizationId,asc"
    ( Page
        "knowledgeModelPackages"
        (PageMetadata 20 3 1 0)
        [ toSuggestion globalKmPackage
        , toSuggestion germanyKmPackage
        , toSuggestion netherlandsKmPackageV2
        ]
    )
  create_test_200
    "HTTP 200 OK (select)"
    appContext
    "/wizard-api/knowledge-model-packages/suggestions?sort=organizationId,asc&select=org.de:core-de:all,org.nl:core-nl:all"
    ( Page
        "knowledgeModelPackages"
        (PageMetadata 20 2 1 0)
        [ toSuggestion germanyKmPackage
        , toSuggestion netherlandsKmPackageV2
        ]
    )
  create_test_200
    "HTTP 200 OK (exclude)"
    appContext
    "/wizard-api/knowledge-model-packages/suggestions?sort=organizationId,asc&exclude=org.de:core-de:all,org.nl:core-nl:all"
    (Page "knowledgeModelPackages" (PageMetadata 20 1 1 0) [toSuggestion globalKmPackage])
  create_test_200
    "HTTP 200 OK (query - q)"
    appContext
    "/wizard-api/knowledge-model-packages/suggestions?q=Germany Knowledge Model"
    (Page "knowledgeModelPackages" (PageMetadata 20 1 1 0) [toSuggestion germanyKmPackage])
  create_test_200
    "HTTP 200 OK (phase)"
    appContext
    "/wizard-api/knowledge-model-packages/suggestions?sort=organizationId,asc&phase=ReleasedKnowledgeModelPackagePhase"
    ( Page
        "knowledgeModelPackages"
        (PageMetadata 20 3 1 0)
        [ toSuggestion globalKmPackage
        , toSuggestion germanyKmPackage
        , toSuggestion netherlandsKmPackageV2
        ]
    )
  create_test_200
    "HTTP 200 OK (query for non-existing)"
    appContext
    "/wizard-api/knowledge-model-packages/suggestions?q=Non-existing Knowledge Model"
    (Page "knowledgeModelPackages" (PageMetadata 20 0 0 0) ([] :: [KnowledgeModelPackageSuggestion]))

create_test_200 title appContext reqUrl expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO KnowledgeModelPackage.runMigration appContext
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
