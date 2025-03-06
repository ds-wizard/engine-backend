module Wizard.Specs.API.Questionnaire.Detail_Content_PUT (
  detail_content_PUT,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireEventDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireVersionDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import WizardLib.Public.Localization.Messages.Public

import SharedTest.Specs.API.Common
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper
import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/questionnaires/{qtnUuid}/content
-- ------------------------------------------------------------------------
detail_content_PUT :: AppContext -> SpecWith ((), Application)
detail_content_PUT appContext =
  describe "PUT /wizard-api/questionnaires/{qtnUuid}/content" $ do
    test_200 appContext
    test_400 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrlT qtnUuid = BS.pack $ "/wizard-api/questionnaires/" ++ U.toString qtnUuid ++ "/content"

reqHeadersT authHeader = reqCtHeader : authHeader

reqDto qtnUuid =
  QuestionnaireContentChangeDTO
    { events = [toEventChangeDTO (slble_rQ2' qtnUuid)]
    }

reqBody qtnUuid = encode (reqDto qtnUuid)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK (Owner, Private)" appContext questionnaire1 questionnaire1EventsEdited [reqAuthHeader]
  create_test_200 "HTTP 200 OK (Owner, VisibleView)" appContext questionnaire2 questionnaire2EventsEdited [reqAuthHeader]
  create_test_200 "HTTP 200 OK (Non-Owner, Public)" appContext questionnaire3 questionnaire3EventsEdited [reqAuthHeader]
  create_test_200 "HTTP 200 OK (Anonymous, Public, Sharing" appContext questionnaire10 questionnaire10EventsEdited []

create_test_200 title appContext qtn qtnEventsEdited authHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT $ qtn.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = reqDto qtn.uuid
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO TML.runMigration appContext
      runInContextIO QTN.runMigration appContext
      runInContextIO (insertQuestionnaire questionnaire7) appContext
      runInContextIO (insertQuestionnaireEvents questionnaire7Events) appContext
      runInContextIO (traverse_ insertQuestionnaireVersion questionnaire7Versions) appContext
      runInContextIO (insertQuestionnaire questionnaire10) appContext
      runInContextIO (insertQuestionnaireEvents questionnaire10Events) appContext
      runInContextIO (traverse_ insertQuestionnaireVersion questionnaire10Versions) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (reqBody qtn.uuid)
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, QuestionnaireContentChangeDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareQuestionnaireDtos resBody expDto
      -- AND: Find a result in DB
      assertExistenceOfQuestionnaireContentInDB appContext qtn.uuid qtnEventsEdited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod (reqUrlT questionnaire3.uuid) "visibility"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    appContext
    questionnaire1
    questionnaire1EventsEdited
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Edit Questionnaire")
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, VisibleView)"
    appContext
    questionnaire2
    questionnaire2EventsEdited
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Edit Questionnaire")
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView, Sharing)"
    appContext
    questionnaire7
    questionnaire7EventsEdited
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, Public)"
    appContext
    questionnaire3
    questionnaire3EventsEdited
    []
    _ERROR_SERVICE_USER__MISSING_USER

create_test_403 title appContext qtn qtnEventsEdited authHeader reason =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT $ qtn.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError reason
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO QTN.runMigration appContext
      runInContextIO (insertQuestionnaire questionnaire7) appContext
      runInContextIO (insertQuestionnaire questionnaire10) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (reqBody qtn.uuid)
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
    "/wizard-api/questionnaires/f08ead5f-746d-411b-aee6-77ea3d24016a/content"
    (reqHeadersT [reqAuthHeader])
    (reqBody $ u' "f08ead5f-746d-411b-aee6-77ea3d24016a")
    "questionnaire"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
