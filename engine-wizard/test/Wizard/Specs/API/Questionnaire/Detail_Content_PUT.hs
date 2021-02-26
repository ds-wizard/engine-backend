module Wizard.Specs.API.Questionnaire.Detail_Content_PUT
  ( detail_content_put
  ) where

import Control.Lens ((^.))
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
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
import Wizard.Api.Resource.Questionnaire.QuestionnaireContentChangeDTO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.Migration.Development.Questionnaire.Data.QuestionnaireEvents
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Service.Questionnaire.Event.QuestionnaireEventMapper
import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /questionnaires/{qtnUuid}/content
-- ------------------------------------------------------------------------
detail_content_put :: AppContext -> SpecWith ((), Application)
detail_content_put appContext =
  describe "PUT /questionnaires/{qtnUuid}/content" $ do
    test_200 appContext
    test_400 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrlT qtnUuid = BS.pack $ "/questionnaires/" ++ U.toString qtnUuid ++ "/content"

reqHeadersT authHeader = reqCtHeader : authHeader

reqDto = QuestionnaireContentChangeDTO {_questionnaireContentChangeDTOEvents = [toEventChangeDTO slble_rQ2']}

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK (Owner, Private)" appContext questionnaire1 questionnaire1ContentEdited [reqAuthHeader]
  create_test_200
    "HTTP 200 OK (Owner, VisibleView)"
    appContext
    questionnaire2
    questionnaire2ContentEdited
    [reqAuthHeader]
  create_test_200
    "HTTP 200 OK (Non-Owner, Public)"
    appContext
    questionnaire3
    questionnaire3ContentEdited
    [reqAuthHeader]

create_test_200 title appContext qtn qtnEdited authHeader =
  it title $
     -- GIVEN: Prepare request
   do
    let reqUrl = reqUrlT $ qtn ^. uuid
    let reqHeaders = reqHeadersT authHeader
     -- AND: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    let expDto = reqDto
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO QTN.runMigration appContext
    runInContextIO (insertQuestionnaire questionnaire7) appContext
    runInContextIO (insertQuestionnaire questionnaire10) appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, QuestionnaireContentChangeDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareQuestionnaireDtos resBody expDto
    -- AND: Find a result in DB
    assertExistenceOfQuestionnaireContentInDB appContext (qtn ^. uuid) qtnEdited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod (reqUrlT $ questionnaire3 ^. uuid) "visibility"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    appContext
    questionnaire1
    questionnaire1ContentEdited
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Edit Questionnaire")
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, VisibleView)"
    appContext
    questionnaire2
    questionnaire2ContentEdited
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Edit Questionnaire")
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView, Sharing)"
    appContext
    questionnaire7
    questionnaire7ContentEdited
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, Public)"
    appContext
    questionnaire3
    questionnaire3ContentEdited
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, Public, Sharing)"
    appContext
    questionnaire10
    questionnaire10ContentEdited
    []
    _ERROR_SERVICE_USER__MISSING_USER

create_test_403 title appContext qtn qtnEdited authHeader reason =
  it title $
     -- GIVEN: Prepare request
   do
    let reqUrl = reqUrlT $ qtn ^. uuid
    let reqHeaders = reqHeadersT authHeader
     -- AND: Prepare expectation
    let expStatus = 403
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = ForbiddenError reason
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO U.runMigration appContext
    runInContextIO QTN.runMigration appContext
    runInContextIO (insertQuestionnaire questionnaire7) appContext
    runInContextIO (insertQuestionnaire questionnaire10) appContext
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
  createNotFoundTest
    reqMethod
    "/questionnaires/f08ead5f-746d-411b-aee6-77ea3d24016a/content"
    (reqHeadersT [reqAuthHeader])
    reqBody
    "questionnaire"
    "f08ead5f-746d-411b-aee6-77ea3d24016a"
