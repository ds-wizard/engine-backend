module Specs.API.Questionnaire.Migration.Current_GET
  ( current_get
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

import Database.DAO.Migration.Questionnaire.MigratorDAO
import Database.DAO.Questionnaire.QuestionnaireDAO
import Database.Migration.Development.Migration.Questionnaire.Data.MigratorStates
import Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified
       Database.Migration.Development.Questionnaire.QuestionnaireMigration
       as QTN
import qualified Database.Migration.Development.User.UserMigration
       as U
import LensesConfig
import Localization
import Model.Context.AppContext
import Model.Error.Error

import Specs.API.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /questionnaires/{qtnUuid}/migrations/current
-- ------------------------------------------------------------------------
current_get :: AppContext -> SpecWith Application
current_get appContext =
  describe "GET /questionnaires/{qtnUuid}/migrations/current" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT qtnUuid = BS.pack $ "/questionnaires/" ++ U.toString qtnUuid ++ "/migrations/current"

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
    "HTTP 200 OK (Non-Owner, PublicReadOnly)"
    appContext
    questionnaire4PublicReadOnly
    questionnaire4PublicReadOnlyUpgraded
    nlQtnMigrationState
    nlQtnMigrationStatePublicReadOnlyDto
    reqNonAdminAuthHeader
  create_test_200
    "HTTP 200 OK (Non-Owner, Public)"
    appContext
    questionnaire4Public
    questionnaire4PublicUpgraded
    nlQtnMigrationState
    nlQtnMigrationStatePublicDto
    reqNonAdminAuthHeader

create_test_200 title appContext oldQtn newQtn state stateDto authHeader =
  it title $
    -- GIVEN: Prepare request
   do
    let reqUrl = reqUrlT $ questionnaire4Upgraded ^. uuid
    let reqHeaders = reqHeadersT reqAuthHeader
    -- AND: Prepare expectation
    let expStatus = 200
    let expDto = stateDto
    let expBody = encode expDto
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    -- AND: Prepare database
    runInContextIO (insertQuestionnaire oldQtn) appContext
    runInContextIO (insertQuestionnaire newQtn) appContext
    runInContextIO (createMigratorState state) appContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod (reqUrlT $ questionnaire4 ^. uuid) [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  createNoPermissionTest (appContext ^. appConfig) reqMethod (reqUrlT $ questionnaire3 ^. uuid) [] "" "QTN_PERM"
  create_test_403 "HTTP 403 FORBIDDEN (Non-Owner, Private)" appContext questionnaire1 "Get Questionnaire"
  create_test_403 "HTTP 403 FORBIDDEN (Non-Owner, PublicReadOnly)" appContext questionnaire2 "Migrate Questionnaire"

create_test_403 title appContext qtn reason =
  it title $
     -- GIVEN: Prepare request
   do
    let reqUrl = reqUrlT $ qtn ^. uuid
    let reqHeaders = reqHeadersT reqNonAdminAuthHeader
     -- AND: Prepare expectation
    let expStatus = 403
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN reason
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO U.runMigration appContext
    runInContextIO QTN.runMigration appContext
    let ms = (nlQtnMigrationState & oldQuestionnaireUuid .~ (qtn ^. uuid)) & newQuestionnaireUuid .~ (qtn ^. uuid)
    runInContextIO (createMigratorState ms) appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext = do
  createNotFoundTest
    reqMethod
    (reqUrlT $ questionnaire4 ^. uuid)
    (reqHeadersT reqAuthHeader)
    reqBody
    "questionnaireMigration"
    "57250a07-a663-4ff3-ac1f-16530f2c1bfe"
