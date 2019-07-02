module Specs.API.Questionnaire.Migration.List_POST
  ( list_post
  ) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import qualified Test.Hspec.Wai.JSON as HJ
import Test.Hspec.Wai.Matcher

import Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO
import Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Api.Resource.Migration.Questionnaire.MigratorStateJM ()
import Database.DAO.Migration.Questionnaire.MigratorDAO
import Database.DAO.Questionnaire.QuestionnaireDAO
import Database.Migration.Development.Migration.Questionnaire.Data.MigratorStates
import Database.Migration.Development.Package.Data.Packages
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
import Specs.API.Questionnaire.Migration.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- POST /questionnaires/{qtnUuid}/migrations
-- ------------------------------------------------------------------------
list_post :: AppContext -> SpecWith Application
list_post appContext =
  describe "POST /questionnaires/{qtnUuid}/migrations" $ do
    test_201 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrlT qtnUuid = BS.pack $ "/questionnaires/" ++ U.toString qtnUuid ++ "/migrations"

reqHeadersT authHeader = [authHeader, reqCtHeader]

reqDto =
  MigratorStateCreateDTO
  { _migratorStateCreateDTOTargetPackageId = netherlandsPackageV2 ^. pId
  , _migratorStateCreateDTOTargetTagUuids = questionnaire4Upgraded ^. selectedTagUuids
  }

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext = do
  create_test_201
    "HTTP 201 CREATED (Owner, Private)"
    appContext
    questionnaire4
    questionnaire4Upgraded
    nlQtnMigrationState
    nlQtnMigrationStateDto
    reqNonAdminAuthHeader
  create_test_201
    "HTTP 201 CREATED (Non-Owner, PublicReadOnly)"
    appContext
    questionnaire4PublicReadOnly
    questionnaire4PublicReadOnlyUpgraded
    nlQtnMigrationState
    nlQtnMigrationStatePublicReadOnlyDto
    reqNonAdminAuthHeader
  create_test_201
    "HTTP 201 CREATED (Non-Owner, Public)"
    appContext
    questionnaire4Public
    questionnaire4PublicUpgraded
    nlQtnMigrationState
    nlQtnMigrationStatePublicDto
    reqNonAdminAuthHeader

create_test_201 title appContext oldQtn newQtn state stateDto authHeader =
  it title $
    -- GIVEN: Prepare request
   do
    let reqUrl = reqUrlT $ oldQtn ^. uuid
    let reqHeaders = reqHeadersT reqAuthHeader
    -- AND: Prepare expectation
    let expStatus = 201
    let expHeaders = resCorsHeadersPlain
    let expDto = stateDto & resolvedQuestionUuids .~ []
    let expBody = encode expDto
    -- AND: Prepare database
    runInContextIO (insertQuestionnaire oldQtn) appContext
    runInContextIO (insertQuestionnaire newQtn) appContext
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, MigratorStateDTO)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareQtnMigratorDtos resBody expDto
    -- AND: Find a result in DB
    let entityInDB =
          (state & newQuestionnaireUuid .~ (resBody ^. newQuestionnaire . uuid)) & resolvedQuestionUuids .~ []
    assertExistenceOfMigrationStateInDB appContext entityInDB

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = do
  createInvalidJsonTest reqMethod (reqUrlT $ questionnaire4 ^. uuid) [HJ.json| { } |] "targetPackageId"
  it "HTTP 400 BAD REQUEST when migration was already created" $
    -- GIVEN: Prepare request
   do
    let reqUrl = reqUrlT $ questionnaire4 ^. uuid
    let reqHeaders = reqHeadersT reqAuthHeader
     -- AND: Prepare expectation
    let expStatus = 400
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = MigratorError _ERROR_SERVICE_MIGRATION_QTN__MIGRATION_UNIQUENESS
    let expBody = encode expDto
    -- AND: Prepare database
    runInContextIO (insertQuestionnaire questionnaire4) appContext
    runInContextIO (insertQuestionnaire questionnaire4Upgraded) appContext
    runInContextIO (createMigratorState nlQtnMigrationState) appContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertCountInDB findMigratorStates appContext 1

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
test_404 appContext = do
  createNotFoundTest
    reqMethod
    (reqUrlT $ questionnaire4 ^. uuid)
    (reqHeadersT reqAuthHeader)
    reqBody
    "questionnaire"
    "57250a07-a663-4ff3-ac1f-16530f2c1bfe"
