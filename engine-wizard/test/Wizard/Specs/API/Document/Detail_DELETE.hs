module Wizard.Specs.API.Document.Detail_DELETE
  ( detail_DELETE
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
import Shared.Model.Error.Error
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Database.Migration.Development.Document.DocumentMigration as DOC_Migration
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN_Migration
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML_Migration
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /documents/{documentId}
-- ------------------------------------------------------------------------
detail_DELETE :: AppContext -> SpecWith ((), Application)
detail_DELETE appContext =
  describe "DELETE /documents/{documentId}" $ do
    test_204 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = "/documents/264ca352-1a99-4ffd-860e-32aee9a98428"

reqHeadersT authHeader = authHeader

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 appContext = do
  create_test_204 "HTTP 204 NO CONTENT (Owner, Private)" appContext questionnaire1 [reqAuthHeader]
  create_test_204 "HTTP 204 NO CONTENT (Non-Owner, VisibleEdit)" appContext questionnaire3 [reqNonAdminAuthHeader]

create_test_204 title appContext qtn authHeader =
  it title $
     -- GIVEN: Prepare request
   do
    let reqHeaders = reqHeadersT authHeader
     -- AND: Prepare expectation
    let expStatus = 204
    let expHeaders = resCtHeader : resCorsHeaders
    let expBody = ""
     -- AND: Run migrations
    runInContextIO U_Migration.runMigration appContext
    runInContextIO TML_Migration.runMigration appContext
    runInContextIO QTN_Migration.runMigration appContext
    runInContextIO (insertQuestionnaire questionnaire10) appContext
    runInContextIO DOC_Migration.runMigration appContext
    runInContextIO (deleteDocumentById (U.toString $ doc1 ^. uuid)) appContext
    runInContextIO (insertDocument (doc1 & questionnaireUuid .~ (qtn ^. uuid))) appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertCountInDB findDocuments appContext 2

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
    runInContextIO (deleteDocumentById (U.toString $ doc1 ^. uuid)) appContext
    runInContextIO (insertDocument (doc1 & questionnaireUuid .~ (qtn ^. uuid))) appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertCountInDB findDocuments appContext 3

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/documents/dc9fe65f-748b-47ec-b30c-d255bbac64a0"
    (reqHeadersT [reqAuthHeader])
    reqBody
    "document"
    [("uuid", "dc9fe65f-748b-47ec-b30c-d255bbac64a0")]
