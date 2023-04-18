module Wizard.Specs.API.Questionnaire.Event.Detail_GET (
  detail_GET,
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
import Shared.Common.Model.Common.Lens
import Shared.Common.Model.Error.Error
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireCommentThreadDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import Wizard.Database.Migration.Development.User.Data.Users
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /questionnaires/{qtnUuid}/events/{eventUuid}
-- ------------------------------------------------------------------------
detail_GET :: AppContext -> SpecWith ((), Application)
detail_GET appContext =
  describe "GET /questionnaires/{qtnUuid}/events/{eventUuid}" $ do
    test_200 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT qtnUuid qtnEventUuid =
  BS.pack $ "/questionnaires/" ++ U.toString qtnUuid ++ "/events/" ++ U.toString qtnEventUuid

reqHeadersT authHeader = authHeader

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK (Owner, Private)" appContext questionnaire1 sre_rQ1' [reqAuthHeader]
  create_test_200 "HTTP 200 OK (Non-Owner, VisibleView)" appContext questionnaire2 sre_rQ1' [reqNonAdminAuthHeader]
  create_test_200
    "HTTP 200 OK (Commentator)"
    appContext
    (questionnaire13 {visibility = PrivateQuestionnaire})
    sre_rQ1'
    [reqNonAdminAuthHeader]
  create_test_200
    "HTTP 200 OK (Non-Commentator, VisibleComment)"
    appContext
    questionnaire13
    sre_rQ1'
    [reqIsaacAuthTokenHeader]
  create_test_200
    "HTTP 200 OK (Anonymous, VisibleComment, AnyoneWithLinkComment)"
    appContext
    (questionnaire13 {sharing = AnyoneWithLinkCommentQuestionnaire})
    sre_rQ1'
    []
  create_test_200 "HTTP 200 OK (Anonymous, VisibleView, Sharing)" appContext questionnaire7 sre_rQ1' []
  create_test_200 "HTTP 200 OK (Non-Owner, VisibleEdit)" appContext questionnaire3 sre_rQ1' [reqNonAdminAuthHeader]
  create_test_200 "HTTP 200 OK (Anonymous, Public, Sharing)" appContext questionnaire10 sre_rQ1' []

create_test_200 title appContext qtn qtnEvent authHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT qtn.uuid (getUuid qtnEvent)
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toEventDTO qtnEvent (Just userAlbert)
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO QTN.runMigration appContext
      runInContextIO deleteQuestionnaireComments appContext
      runInContextIO deleteQuestionnaireCommentThreads appContext
      runInContextIO deleteQuestionnaires appContext
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
test_403 appContext = do
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    appContext
    questionnaire1
    sre_rQ1'
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "View Questionnaire")
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView)"
    appContext
    questionnaire2
    sre_rQ1'
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, Public)"
    appContext
    questionnaire3
    sre_rQ1'
    []
    _ERROR_SERVICE_USER__MISSING_USER

create_test_403 title appContext qtn qtnEvent authHeader errorMessage =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT qtn.uuid (getUuid qtnEvent)
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
    "/questionnaires/f08ead5f-746d-411b-aee6-77ea3d24016a/events"
    [reqHeadersT reqAuthHeader]
    reqBody
    "questionnaire"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
