module Wizard.Specs.API.Document.Detail_Available_Submission_Services_GET (
  detail_available_submission_Services_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Database.Migration.Development.Document.DocumentMigration as DOC_Migration
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN_Migration
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Service.Submission.SubmissionMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------------------
-- GET /wizard-api/documents/{docUuid}/available-submission-services
-- ------------------------------------------------------------------------------------
detail_available_submission_Services_GET :: AppContext -> SpecWith ((), Application)
detail_available_submission_Services_GET appContext =
  describe "GET /wizard-api/documents/{docUuid}/available-submission-services" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/documents/264ca352-1a99-4ffd-860e-32aee9a98428/available-submission-services"

reqHeadersT authHeader = authHeader

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK (Owner, Private)" appContext questionnaire1 [reqAuthHeader]
  create_test_200 "HTTP 200 OK (Non-Owner, VisibleEdit)" appContext questionnaire3 [reqNonAdminAuthHeader]

create_test_200 title appContext qtn authHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = [toSubmissionServiceSimpleDTO defaultSubmissionService]
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration appContext
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO QTN_Migration.runMigration appContext
      runInContextIO (insertQuestionnaire questionnaire10) appContext
      runInContextIO DOC_Migration.runMigration appContext
      runInContextIO (deleteDocumentByUuid doc1.uuid) appContext
      runInContextIO (insertDocument (doc1 {questionnaireUuid = qtn.uuid})) appContext
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
test_403 appContext = do
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    appContext
    questionnaire1
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Edit Questionnaire")
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, VisibleView)"
    appContext
    questionnaire2
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Edit Questionnaire")

create_test_403 title appContext qtn authHeader errorMessage =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError errorMessage
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration appContext
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO QTN_Migration.runMigration appContext
      runInContextIO DOC_Migration.runMigration appContext
      runInContextIO (insertQuestionnaire questionnaire7) appContext
      runInContextIO (deleteDocumentByUuid doc1.uuid) appContext
      runInContextIO (insertDocument (doc1 {questionnaireUuid = qtn.uuid})) appContext
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
  createNotFoundTest'
    reqMethod
    "/wizard-api/documents/dc9fe65f-748b-47ec-b30c-d255bbac64a0/available-submission-services"
    (reqHeadersT [reqAuthHeader])
    reqBody
    "document"
    [("uuid", "dc9fe65f-748b-47ec-b30c-d255bbac64a0")]
