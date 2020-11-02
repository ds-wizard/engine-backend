module Wizard.Specs.API.Submission.List_POST
  ( list_post
  ) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (encode)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig hiding (request)
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Localization.Messages.Public
import Wizard.Api.Resource.Submission.SubmissionCreateJM ()
import Wizard.Api.Resource.Submission.SubmissionJM ()
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.Migration.Development.Document.Data.Documents
import qualified Wizard.Database.Migration.Development.Document.DocumentMigration as DOC_Migration
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.Submission.Data.Submissions
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import SharedTest.Specs.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /submissions
-- ------------------------------------------------------------------------
list_post :: AppContext -> SpecWith ((), Application)
list_post appContext =
  describe "POST /submissions" $ do
    test_201 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/submissions"

reqHeadersT authHeader = reqCtHeader : authHeader

reqDto = submissionCreate

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext = do
  create_test_201 "HTTP 201 CREATED (Owner, Private)" appContext questionnaire1 [reqAuthHeader]
  create_test_201 "HTTP 201 CREATED (Non-Owner, VisibleEdit)" appContext questionnaire3 [reqNonAdminAuthHeader]
  create_test_201 "HTTP 201 CREATED (Anonymous, Public, Sharing)" appContext questionnaire10 []

create_test_201 title appContext qtn authHeader =
  it title $
     -- GIVEN: Prepare request
   do
    let reqHeaders = reqHeadersT authHeader
     -- AND: Prepare expectation
    let expStatus = 201
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = submission1
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO U_Migration.runMigration appContext
    runInContextIO DOC_Migration.runMigration appContext
    runInContextIO (insertQuestionnaire qtn) appContext
    runInContextIO (deleteDocumentById (U.toString $ doc1 ^. uuid)) appContext
    runInContextIO (insertDocument (doc1 & questionnaireUuid .~ (qtn ^. uuid))) appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

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
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView)"
    appContext
    questionnaire2
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView, Sharing)"
    appContext
    questionnaire7
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, Public)"
    appContext
    questionnaire3
    []
    _ERROR_SERVICE_USER__MISSING_USER

create_test_403 title appContext qtn authHeader errorMessage =
  it title $
     -- GIVEN: Prepare request
   do
    let reqHeaders = reqHeadersT authHeader
     -- AND: Prepare expectation
    let expStatus = 403
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = createForbiddenError errorMessage
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO U_Migration.runMigration appContext
    runInContextIO DOC_Migration.runMigration appContext
    runInContextIO (insertQuestionnaire qtn) appContext
    runInContextIO (deleteDocumentById (U.toString $ doc1 ^. uuid)) appContext
    runInContextIO (insertDocument (doc1 & questionnaireUuid .~ (qtn ^. uuid))) appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
