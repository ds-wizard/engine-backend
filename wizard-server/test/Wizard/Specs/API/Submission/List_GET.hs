module Wizard.Specs.API.Submission.List_GET (
  list_GET,
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
import Wizard.Api.Resource.Submission.SubmissionJM ()
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.Migration.Development.Document.Data.Documents
import qualified Wizard.Database.Migration.Development.Document.DocumentMigration as DOC_Migration
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN_Migration
import Wizard.Database.Migration.Development.Submission.Data.Submissions
import qualified Wizard.Database.Migration.Development.Submission.SubmissionMigration as SUB_Migration
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.Model.Questionnaire.Questionnaire

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/documents/{docUuid}/submissions
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext =
  describe "GET /wizard-api/documents/{docUuid}/submissions" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/documents/264ca352-1a99-4ffd-860e-32aee9a98428/submissions"

reqHeadersT authHeader = reqCtHeader : authHeader

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK (Owner, Private)" appContext questionnaire1 [reqAuthHeader]
  create_test_200 "HTTP 200 OK (Non-Owner, VisibleEdit)" appContext questionnaire3 [reqNonAdminAuthHeader]
  create_test_200 "HTTP 200 OK (Non-Owner, VisibleView)" appContext questionnaire2 [reqNonAdminAuthHeader]

create_test_200 title appContext qtn authHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = [submission1Dto]
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration appContext
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO QTN_Migration.runMigration appContext
      runInContextIO (insertQuestionnaire questionnaire10) appContext
      runInContextIO DOC_Migration.runMigration appContext
      runInContextIO (deleteDocumentByUuid doc1.uuid) appContext
      runInContextIO (insertDocument (doc1 {questionnaireUuid = qtn.uuid})) appContext
      runInContextIO SUB_Migration.runMigration appContext
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
test_403 appContext =
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    appContext
    questionnaire1
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "View Questionnaire")

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
      runInContextIO (insertQuestionnaire questionnaire7) appContext
      runInContextIO DOC_Migration.runMigration appContext
      runInContextIO (deleteDocumentByUuid doc1.uuid) appContext
      runInContextIO (insertDocument (doc1 {questionnaireUuid = qtn.uuid})) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
