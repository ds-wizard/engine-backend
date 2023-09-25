module Wizard.Specs.API.Questionnaire.ProjectTag.List_Suggestions_GET (
  list_suggestions_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN_Migration
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/questionnaires/project-tags/suggestions
-- ------------------------------------------------------------------------
list_suggestions_GET :: AppContext -> SpecWith ((), Application)
list_suggestions_GET appContext =
  describe "GET /wizard-api/questionnaires/project-tags/suggestions" $ do
    test_200 appContext
    test_401 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/questionnaires/project-tags/suggestions"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK (All)"
    appContext
    "/wizard-api/questionnaires/project-tags/suggestions?sort=projectTag,asc"
    ( Page
        "projectTags"
        (PageMetadata 20 4 1 0)
        [_QUESTIONNAIRE_PROJECT_TAG_1, _QUESTIONNAIRE_PROJECT_TAG_2, _SETTINGS_PROJECT_TAG_1, _SETTINGS_PROJECT_TAG_2]
    )
  create_test_200
    "HTTP 200 OK (pagination)"
    appContext
    "/wizard-api/questionnaires/project-tags/suggestions?sort=projectTag,asc&page=1&size=1"
    (Page "projectTags" (PageMetadata 1 4 4 1) [_QUESTIONNAIRE_PROJECT_TAG_2])
  create_test_200
    "HTTP 200 OK (query)"
    appContext
    "/wizard-api/questionnaires/project-tags/suggestions?sort=projectTag,asc&q=settingsProject"
    (Page "projectTags" (PageMetadata 20 2 1 0) [_SETTINGS_PROJECT_TAG_1, _SETTINGS_PROJECT_TAG_2])
  create_test_200
    "HTTP 200 OK (exclude)"
    appContext
    "/wizard-api/questionnaires/project-tags/suggestions?sort=projectTag,asc&exclude=settingsProjectTag2"
    ( Page
        "projectTags"
        (PageMetadata 20 3 1 0)
        [_QUESTIONNAIRE_PROJECT_TAG_1, _QUESTIONNAIRE_PROJECT_TAG_2, _SETTINGS_PROJECT_TAG_1]
    )
  create_test_200
    "HTTP 200 OK (query, exclude)"
    appContext
    "/wizard-api/questionnaires/project-tags/suggestions?sort=projectTag,asc&q=settings&exclude=settingsProjectTag2"
    (Page "projectTags" (PageMetadata 20 1 1 0) [_SETTINGS_PROJECT_TAG_1])

create_test_200 title appContext reqUrl expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO QTN_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody
