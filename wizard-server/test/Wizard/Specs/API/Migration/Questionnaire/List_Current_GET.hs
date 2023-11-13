module Wizard.Specs.API.Migration.Questionnaire.List_Current_GET (
  list_current_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Migration.Questionnaire.Data.MigratorStates
import qualified Wizard.Database.Migration.Development.Migration.Questionnaire.MigratorMigration as QTN_MIG
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Model.Migration.Questionnaire.MigratorState
import Wizard.Model.Questionnaire.Questionnaire

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/questionnaires/{qtnUuid}/migrations/current
-- ------------------------------------------------------------------------
list_current_GET :: AppContext -> SpecWith ((), Application)
list_current_GET appContext =
  describe "GET /wizard-api/questionnaires/{qtnUuid}/migrations/current" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT qtnUuid = BS.pack $ "/wizard-api/questionnaires/" ++ U.toString qtnUuid ++ "/migrations/current"

reqHeadersT authHeader = [authHeader, reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK (Owner, Private)"
    appContext
    questionnaire4
    questionnaire4Upgraded
    nlQtnMigrationState
    nlQtnMigrationStateDto
    reqNonAdminAuthHeader
  create_test_200
    "HTTP 200 OK (Non-Owner, VisibleView)"
    appContext
    questionnaire4VisibleView
    questionnaire4VisibleViewUpgraded
    nlQtnMigrationState
    nlQtnMigrationStateVisibleViewDto
    reqNonAdminAuthHeader
  create_test_200
    "HTTP 200 OK (Non-Owner, Public)"
    appContext
    questionnaire4VisibleEdit
    questionnaire4VisibleEditUpgraded
    nlQtnMigrationState
    nlQtnMigrationStateVisibleEditDto
    reqNonAdminAuthHeader

create_test_200 title appContext oldQtn newQtn state stateDto authHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT $ questionnaire4Upgraded.uuid
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expDto = stateDto
      let expBody = encode expDto
      let expHeaders = resCtHeader : resCorsHeaders
      -- AND: Prepare database
      runInContextIO TML.runMigration appContext
      runInContextIO (insertQuestionnaire oldQtn) appContext
      runInContextIO (insertQuestionnaire newQtn) appContext
      runInContextIO (insertQuestionnaire differentQuestionnaire) appContext
      runInContextIO QTN_MIG.runMigration appContext
      runInContextIO (insertMigratorState state) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod (reqUrlT questionnaire4.uuid) [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  createNoPermissionTest appContext reqMethod (reqUrlT questionnaire3.uuid) [] "" "QTN_PERM"
  create_test_403 "HTTP 403 FORBIDDEN (Non-Owner, Private)" appContext questionnaire1 "View Questionnaire"
  create_test_403 "HTTP 403 FORBIDDEN (Non-Owner, VisibleView)" appContext questionnaire2 "Migrate Questionnaire"

create_test_403 title appContext qtn reason =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT $ qtn.uuid
      let reqHeaders = reqHeadersT reqNonAdminAuthHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN reason
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO QTN.runMigration appContext
      let ms = nlQtnMigrationState {oldQuestionnaireUuid = qtn.uuid, newQuestionnaireUuid = qtn.uuid}
      runInContextIO (insertMigratorState ms) appContext
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
    (reqUrlT questionnaire4.uuid)
    (reqHeadersT reqAuthHeader)
    reqBody
    "questionnaire_migration"
    [("new_questionnaire_uuid", "57250a07-a663-4ff3-ac1f-16530f2c1bfe")]
