module Wizard.Specs.API.Questionnaire.Comment.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentThreadDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireComments
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireComment
import Wizard.Service.Questionnaire.Comment.QuestionnaireCommentMapper
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.Public.Localization.Messages.Public

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/questionnaires/{qtnUuid}/comments
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext =
  describe "GET /wizard-api/questionnaires/{qtnUuid}/comments" $ do
    test_200 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT qtnUuid = BS.pack $ "/wizard-api/questionnaires/" ++ U.toString qtnUuid ++ "/comments"

reqHeadersT authHeader = authHeader

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK (Owner, Private)"
    appContext
    questionnaire1
    [reqAuthHeader]
    [qtn1AlbertEditQtnPermDto]
  create_test_200
    "HTTP 200 OK (Commenter)"
    appContext
    (questionnaire13 {visibility = PrivateQuestionnaire})
    [reqNonAdminAuthHeader]
    [qtn13NikolaCommentQtnPermDto]
  create_test_200
    "HTTP 200 OK (Non-Commenter, VisibleComment)"
    appContext
    questionnaire13
    [reqIsaacAuthTokenHeader]
    [qtn13NikolaCommentQtnPermDto]
  create_test_200
    "HTTP 200 OK (Anonymous, VisibleComment, AnyoneWithLinkComment)"
    appContext
    (questionnaire13 {sharing = AnyoneWithLinkCommentQuestionnaire})
    []
    [qtn13NikolaCommentQtnPermDto]
  create_test_200
    "HTTP 200 OK (Non-Owner, VisibleEdit)"
    appContext
    questionnaire3
    [reqNonAdminAuthHeader]
    []
  create_test_200
    "HTTP 200 OK (Anonymous, Public, Sharing)"
    appContext
    questionnaire10
    []
    []

create_test_200 title appContext qtn authHeader permissions =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT qtn.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO (insertPackage germanyPackage) appContext
      thread1 <- liftIO . create_cmtQ1_t1 $ qtn.uuid
      comment1 <- liftIO . create_cmtQ1_t1_1 $ thread1.uuid
      comment2 <- liftIO . create_cmtQ1_t1_2 $ thread1.uuid
      runInContextIO (insertQuestionnaire qtn) appContext
      runInContextIO (insertQuestionnaireCommentThread thread1) appContext
      runInContextIO (insertQuestionnaireComment comment1) appContext
      runInContextIO (insertQuestionnaireComment comment2) appContext
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = M.fromList [(cmtQ1_path, [toCommentThreadList thread1 Nothing (Just userAlbert) (L.sort [toCommentList comment1 (Just userAlbert), toCommentList comment2 (Just userAlbert)])])]
      let expBody = encode expDto
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
    (_ERROR_VALIDATION__FORBIDDEN "Comment Questionnaire")
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, VisibleView)"
    appContext
    questionnaire2
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Comment Questionnaire")
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView, Sharing)"
    appContext
    questionnaire7
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView)"
    appContext
    questionnaire2
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
      let reqUrl = reqUrlT qtn.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError errorMessage
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO (insertPackage germanyPackage) appContext
      runInContextIO (insertQuestionnaire qtn) appContext
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
    "/wizard-api/questionnaires/f08ead5f-746d-411b-aee6-77ea3d24016a/comments"
    [reqHeadersT reqAuthHeader]
    reqBody
    "questionnaire"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
