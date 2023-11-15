module Wizard.Specs.API.QuestionnaireAction.List_Suggestions_GET (
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
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN_Migration
import Wizard.Database.Migration.Development.QuestionnaireAction.Data.QuestionnaireActions
import qualified Wizard.Database.Migration.Development.QuestionnaireAction.QuestionnaireActionMigration as QA_Migration
import Wizard.Model.Context.AppContext
import Wizard.Service.QuestionnaireAction.QuestionnaireActionMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/questionnaire-actions/suggestions
-- ------------------------------------------------------------------------
list_suggestions_GET :: AppContext -> SpecWith ((), Application)
list_suggestions_GET appContext =
  describe "GET /wizard-api/questionnaire-actions/suggestions" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/questionnaire-actions/suggestions"

reqHeadersT reqAuthHeader = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK"
    appContext
    "/wizard-api/questionnaire-actions/suggestions?enabled=true"
    reqAuthHeader
    (Page "questionnaireActions" (PageMetadata 20 2 1 0) [questionnaireActionFtp2, questionnaireActionMail1])
  create_test_200
    "HTTP 200 OK (query 'q')"
    appContext
    "/wizard-api/questionnaire-actions/suggestions?enabled=true&q=FTP"
    reqAuthHeader
    (Page "questionnaireActions" (PageMetadata 20 1 1 0) [questionnaireActionFtp2])
  create_test_200
    "HTTP 200 OK (query 'q' for non-existing)"
    appContext
    "/wizard-api/questionnaire-actions/suggestions?enabled=true&q=Non-existing Questionnaire Report"
    reqAuthHeader
    (Page "questionnaireActions" (PageMetadata 20 0 0 0) [])
  create_test_200
    "HTTP 200 OK (query 'questionnaireUuid')"
    appContext
    "/wizard-api/questionnaire-actions/suggestions?enabled=true&questionnaireUuid=af984a75-56e3-49f8-b16f-d6b99599910a"
    reqAuthHeader
    (Page "questionnaireActions" (PageMetadata 20 1 1 0) [questionnaireActionFtp2])

create_test_200 title appContext reqUrl reqAuthHeader expEntities =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = fmap toDTO expEntities
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO QTN_Migration.runMigration appContext
      runInContextIO QA_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher = ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [] reqBody "QTN_PERM"
