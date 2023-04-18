module Wizard.Specs.API.Questionnaire.Detail_Documents_GET (
  detail_documents_get,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Document.DocumentJM ()
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.Migration.Development.Document.Data.Documents
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN_Migration
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.User.User
import Wizard.S3.Document.DocumentS3
import Wizard.Service.Document.DocumentMapper
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /questionnaires/{qtnUuid}/documents
-- ------------------------------------------------------------------------
detail_documents_get :: AppContext -> SpecWith ((), Application)
detail_documents_get appContext =
  describe "GET /questionnaires/{qtnUuid}/documents" $ do
    test_200 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT qtnUuid = BS.pack $ "/questionnaires/" ++ U.toString qtnUuid ++ "/documents?sort=name,asc"

reqHeadersT authHeader = authHeader

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 CREATED (Owner)" appContext [reqAuthHeader]
  create_test_200 "HTTP 200 CREATED (Non-Owner)" appContext [reqNonAdminAuthHeader]
  create_test_200 "HTTP 200 CREATED (Anonymous)" appContext []

create_test_200 title appContext authHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT questionnaire6.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration appContext
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO QTN_Migration.runMigration appContext
      runInContextIO (insertQuestionnaire questionnaire6) appContext
      runInContextIO deleteDocuments appContext
      runInContextIO removeDocumentContents appContext
      runInContextIO (insertDocument (doc1 {questionnaireUuid = questionnaire6.uuid})) appContext
      runInContextIO (insertDocument (doc2 {questionnaireUuid = questionnaire6.uuid, creatorUuid = Just userIsaac.uuid})) appContext
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            Page
              "documents"
              (PageMetadata 20 2 1 0)
              [toDTO doc1 (Just questionnaire6Simple) [], toDTO (doc2 {creatorUuid = Just userIsaac.uuid}) (Just questionnaire6Simple) []]
      let expBody = encode (fmap (\x -> x wizardDocumentTemplate) expDto)
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
    (_ERROR_VALIDATION__FORBIDDEN "View Questionnaire")
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView)"
    appContext
    questionnaire2
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleEdit)"
    appContext
    questionnaire3
    []
    _ERROR_SERVICE_USER__MISSING_USER

create_test_403 title appContext qtn authHeader errorMessage =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT qtn.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError errorMessage
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO TML_Migration.runMigration appContext
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
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/questionnaires/f08ead5f-746d-411b-aee6-77ea3d24016a/documents"
    (reqHeadersT [reqAuthHeader])
    reqBody
    "questionnaire"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
