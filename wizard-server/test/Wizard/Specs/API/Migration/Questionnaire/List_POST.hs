module Wizard.Specs.API.Migration.Questionnaire.List_POST (
  list_POST,
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
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDetailQuestionnaireDTO
import Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireEventDAO
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
import Wizard.Specs.API.Migration.Questionnaire.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/questionnaires/{qtnUuid}/migrations
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /wizard-api/questionnaires/{qtnUuid}/migrations" $ do
    test_201 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrlT qtnUuid = BS.pack $ "/wizard-api/questionnaires/" ++ U.toString qtnUuid ++ "/migrations"

reqHeadersT authHeader = [authHeader, reqCtHeader]

reqDto = migratorStateCreate

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext = do
  create_test_201
    "HTTP 201 CREATED (Owner, Private)"
    appContext
    questionnaire4
    questionnaire4Events
    questionnaire4Upgraded
    questionnaire4UpgradedEvents
    nlQtnMigrationState
    nlQtnMigrationStateDto
    reqNonAdminAuthHeader
  create_test_201
    "HTTP 201 CREATED (Non-Owner, VisibleView)"
    appContext
    questionnaire4VisibleView
    questionnaire4VisibleViewEvents
    questionnaire4VisibleViewUpgraded
    questionnaire4VisibleViewUpgradedEvents
    nlQtnMigrationState
    nlQtnMigrationStateVisibleViewDto
    reqNonAdminAuthHeader
  create_test_201
    "HTTP 201 CREATED (Non-Owner, Public)"
    appContext
    questionnaire4VisibleEdit
    questionnaire4VisibleEditEvents
    questionnaire4VisibleEditUpgraded
    questionnaire4VisibleEditUpgradedEvents
    nlQtnMigrationState
    nlQtnMigrationStateVisibleEditDto
    reqNonAdminAuthHeader

create_test_201 title appContext oldQtn oldQtnEvents newQtn newQtnEvents state stateDto authHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT $ oldQtn.uuid
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCorsHeadersPlain
      let expDto = stateDto {resolvedQuestionUuids = []} :: MigratorStateDTO
      let expBody = encode expDto
      -- AND: Prepare database
      runInContextIO TML.runMigration appContext
      runInContextIO (insertQuestionnaire oldQtn) appContext
      runInContextIO (insertQuestionnaireEvents oldQtnEvents) appContext
      runInContextIO (insertQuestionnaire newQtn) appContext
      runInContextIO (insertQuestionnaireEvents newQtnEvents) appContext
      runInContextIO (insertQuestionnaire differentQuestionnaire) appContext
      runInContextIO (insertQuestionnaireEvents differentQuestionnaireEvents) appContext
      runInContextIO QTN_MIG.runMigration appContext
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, MigratorStateDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareQtnMigratorDtos resBody expDto
      -- AND: Find a result in DB
      let entityInDB =
            state
              { newQuestionnaireUuid = resBody.newQuestionnaire.uuid
              , resolvedQuestionUuids = []
              }
      assertExistenceOfMigrationStateInDB appContext entityInDB

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod (reqUrlT questionnaire4.uuid) "targetPackageId"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod (reqUrlT questionnaire4.uuid) [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  createNoPermissionTest appContext reqMethod (reqUrlT questionnaire3.uuid) [reqCtHeader] reqBody "QTN_PERM"
  create_test_403 "HTTP 403 FORBIDDEN (Non-Owner, Private)" appContext questionnaire1 "Migrate Questionnaire"
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
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findMigratorStates appContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    (reqUrlT questionnaire4.uuid)
    (reqHeadersT reqAuthHeader)
    reqBody
    "questionnaire"
    [("uuid", "57250a07-a663-4ff3-ac1f-16530f2c1bfe")]
