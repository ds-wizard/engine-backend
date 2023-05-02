module Wizard.Specs.API.Questionnaire.Detail_GET (
  detail_get,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
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
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireReplies
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireVersions
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireComment
import Wizard.Model.Questionnaire.QuestionnaireState
import Wizard.Service.Questionnaire.Comment.QuestionnaireCommentMapper
import Wizard.Service.Questionnaire.QuestionnaireMapper
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import qualified WizardLib.KnowledgeModel.Service.Package.PackageMapper as SPM

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /questionnaires/{qtnUuid}
-- ------------------------------------------------------------------------
detail_get :: AppContext -> SpecWith ((), Application)
detail_get appContext =
  describe "GET /questionnaires/{qtnUuid}" $ do
    test_200 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT qtnUuid = BS.pack $ "/questionnaires/" ++ U.toString qtnUuid

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
    questionnaire1Ctn
    True
    [reqAuthHeader]
    [qtn1AlbertEditPermRecordDto]
  create_test_200
    "HTTP 200 OK (Non-Owner, VisibleView)"
    appContext
    questionnaire2
    questionnaire2Ctn
    False
    [reqNonAdminAuthHeader]
    [qtn2AlbertEditPermRecordDto]
  create_test_200
    "HTTP 200 OK (Commentator)"
    appContext
    (questionnaire13 {visibility = PrivateQuestionnaire})
    questionnaire13Ctn
    True
    [reqNonAdminAuthHeader]
    [qtn13NikolaCommentPermRecordDto]
  create_test_200
    "HTTP 200 OK (Non-Commentator, VisibleComment)"
    appContext
    questionnaire13
    questionnaire13Ctn
    True
    [reqIsaacAuthTokenHeader]
    [qtn13NikolaCommentPermRecordDto]
  create_test_200
    "HTTP 200 OK (Anonymous, VisibleComment, AnyoneWithLinkComment)"
    appContext
    (questionnaire13 {sharing = AnyoneWithLinkCommentQuestionnaire})
    questionnaire13Ctn
    True
    []
    [qtn13NikolaCommentPermRecordDto]
  create_test_200
    "HTTP 200 OK (Anonymous, VisibleView, Sharing)"
    appContext
    questionnaire7
    questionnaire7Ctn
    False
    []
    [qtn7AlbertEditPermRecordDto]
  create_test_200
    "HTTP 200 OK (Non-Owner, VisibleEdit)"
    appContext
    questionnaire3
    questionnaire3Ctn
    True
    [reqNonAdminAuthHeader]
    []
  create_test_200 "HTTP 200 OK (Anonymous, Public, Sharing)" appContext questionnaire10 questionnaire10Ctn True [] []

create_test_200 title appContext qtn qtnCtn showComments authHeader permissions =
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
      let commentThreadsMap =
            if showComments
              then
                M.fromList
                  [
                    ( cmtQ1_path
                    ,
                      [ toCommentThreadDTO
                          thread1
                          (Just userAlbert)
                          [toCommentDTO comment1 (Just userAlbert), toCommentDTO comment2 (Just userAlbert)]
                      ]
                    )
                  ]
              else M.empty
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            toDetailWithPackageWithEventsDTO
              qtn
              qtnCtn
              (SPM.toPackage germanyPackage)
              km1WithQ4
              QSDefault
              (Just wizardDocumentTemplate)
              (Just formatJson)
              fReplies
              commentThreadsMap
              permissions
              qVersionsDto
              Nothing
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
    (_ERROR_VALIDATION__FORBIDDEN "View Questionnaire")
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
    "/questionnaires/f08ead5f-746d-411b-aee6-77ea3d24016a"
    [reqHeadersT reqAuthHeader]
    reqBody
    "questionnaire"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
