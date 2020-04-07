module Wizard.Specs.API.Questionnaire.List_POST_CloneUuid
  ( list_post_cloneUuid
  ) where

import Control.Lens ((&), (.~), (^.))
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
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext

import SharedTest.Specs.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /questionnaires?cloneUuid={qtnUuid}
-- ------------------------------------------------------------------------
list_post_cloneUuid :: AppContext -> SpecWith Application
list_post_cloneUuid appContext =
  describe "POST /questionnaires?cloneUuid={qtnUuid}" $ do
    test_201 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrlT qtnUuid = BS.pack $ "/questionnaires?cloneUuid=" ++ U.toString qtnUuid

reqHeadersT authHeader = [authHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext = do
  create_test_201 "HTTP 200 OK (Owner, Private)" appContext questionnaire1Dto
  create_test_201 "HTTP 200 OK (Owner, PublicReadOnly)" appContext questionnaire2Dto
  create_test_201 "HTTP 200 OK (Non-Owner, Public)" appContext questionnaire3Dto

create_test_201 title appContext qtn =
  it title $
     -- GIVEN: Prepare request
   do
    let reqUrl = reqUrlT $ qtn ^. uuid
    let reqHeaders = reqHeadersT reqAuthHeader
     -- AND: Prepare expectation
    let expStatus = 201
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    let expDto = qtn & level .~ 1
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO QTN.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, QuestionnaireDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareQuestionnaireCloneDtos resBody expDto
    -- AND: Find a result in DB
    assertCountInDB findQuestionnaires appContext 4

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod (reqUrlT $ questionnaire3 ^. uuid) [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  createNoPermissionTest (appContext ^. serverConfig) reqMethod (reqUrlT $ questionnaire3 ^. uuid) [] reqBody "QTN_PERM"
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    appContext
    questionnaire1
    questionnaire1Edited
    "Get Questionnaire"

create_test_403 title appContext qtn qtnEdited reason =
  it title $
     -- GIVEN: Prepare request
   do
    let reqUrl = reqUrlT $ qtn ^. uuid
    let reqHeaders = reqHeadersT reqNonAdminAuthHeader
     -- AND: Prepare expectation
    let expStatus = 403
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = createForbiddenError $ _ERROR_VALIDATION__FORBIDDEN reason
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO U.runMigration appContext
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
  createNotFoundTest
    reqMethod
    "/questionnaires?cloneUuid=f08ead5f-746d-411b-aee6-77ea3d24016a"
    (reqHeadersT reqAuthHeader)
    reqBody
    "questionnaire"
    "f08ead5f-746d-411b-aee6-77ea3d24016a"
