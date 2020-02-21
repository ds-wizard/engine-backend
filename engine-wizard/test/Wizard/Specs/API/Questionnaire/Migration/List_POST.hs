module Wizard.Specs.API.Questionnaire.Migration.List_POST
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

import LensesConfig
import Shared.Localization.Messages.Public
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateCreateDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateDTO
import Wizard.Api.Resource.Migration.Questionnaire.MigratorStateJM ()
import Wizard.Database.DAO.Migration.Questionnaire.MigratorDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.Migration.Development.Migration.Questionnaire.Data.MigratorStates
import Wizard.Database.Migration.Development.Package.Data.Packages
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext

import SharedTest.Specs.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.Migration.Common
import Wizard.Specs.Common

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
test_400 appContext =
  createInvalidJsonTest reqMethod (reqUrlT $ questionnaire4 ^. uuid) [HJ.json| { } |] "targetPackageId"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod (reqUrlT $ questionnaire4 ^. uuid) [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  createNoPermissionTest
    (appContext ^. applicationConfig)
    reqMethod
    (reqUrlT $ questionnaire3 ^. uuid)
    [reqCtHeader]
    reqBody
    "QTN_PERM"
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
