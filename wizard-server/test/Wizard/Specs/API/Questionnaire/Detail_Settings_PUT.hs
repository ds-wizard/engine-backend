module Wizard.Specs.API.Questionnaire.Detail_Settings_PUT (
  detail_settings_PUT,
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
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Questionnaire.QuestionnaireSettingsChangeDTO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireEventDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/questionnaires/{qtnUuid}/settings
-- ------------------------------------------------------------------------
detail_settings_PUT :: AppContext -> SpecWith ((), Application)
detail_settings_PUT appContext =
  describe "PUT /wizard-api/questionnaires/{qtnUuid}/settings" $ do
    test_200 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrlT qtnUuid = BS.pack $ "/wizard-api/questionnaires/" ++ U.toString qtnUuid ++ "/settings"

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqDtoT qtn =
  QuestionnaireSettingsChangeDTO
    { name = qtn.name
    , description = qtn.description
    , projectTags = qtn.projectTags
    , documentTemplateId = qtn.documentTemplateId
    , formatUuid = qtn.formatUuid
    , isTemplate = qtn.isTemplate
    }

reqBodyT qtn = encode $ reqDtoT qtn

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK (Owner, Private)"
    appContext
    questionnaire1
    questionnaire1SettingsEdited
    questionnaire1Events
    questionnaire1Ctn
    []
    True
    [reqAuthHeader]
    False
  create_test_200
    "HTTP 200 OK (Owner, VisibleView)"
    appContext
    questionnaire2
    questionnaire2SettingsEdited
    questionnaire2Events
    questionnaire2Ctn
    []
    False
    [reqAuthHeader]
    False
  create_test_200
    "HTTP 200 OK (Non-Owner, Private, Sharing, Anonymous Enabled)"
    appContext
    questionnaire10
    questionnaire10EditedSettings
    questionnaire10Events
    questionnaire10Ctn
    [qtn10NikolaEditQtnPermDto]
    False
    [reqNonAdminAuthHeader]
    True

create_test_200 title appContext qtn qtnEdited qtnEvents qtnCtn permissions showComments authHeader anonymousEnabled =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT $ qtn.uuid
      let reqHeaders = reqHeadersT authHeader
      let reqBody = reqBodyT qtnEdited
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = reqDtoT qtnEdited
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO QTN.runMigration appContext
      runInContextIO (insertQuestionnaire questionnaire10) appContext
      runInContextIO (insertQuestionnaireEvents questionnaire10Events) appContext
      -- AND: Enabled anonymous sharing
      updateAnonymousQuestionnaireSharing appContext anonymousEnabled
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find a result in DB
      assertExistenceOfQuestionnaireInDB appContext qtnEdited qtnEvents

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod (reqUrlT questionnaire3.uuid) "visibility"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext =
  createAuthTest reqMethod (reqUrlT questionnaire3.uuid) [reqCtHeader] (reqBodyT questionnaire1)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  createNoPermissionTest
    appContext
    reqMethod
    (reqUrlT questionnaire3.uuid)
    [reqCtHeader]
    (reqBodyT questionnaire1)
    "QTN_PERM"
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    appContext
    questionnaire1
    questionnaire1SettingsEdited
    "Administrate Questionnaire"
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, VisibleView)"
    appContext
    questionnaire2
    questionnaire2SettingsEdited
    "Administrate Questionnaire"
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private, Sharing, Anonymous Disabled)"
    appContext
    questionnaire10
    questionnaire10EditedSettings
    "Administrate Questionnaire"

create_test_403 title appContext qtn qtnEdited reason =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT $ qtn.uuid
      let reqHeaders = reqHeadersT [reqNonAdminAuthHeader]
      let reqBody = reqBodyT qtnEdited
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN reason
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO QTN.runMigration appContext
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
  createNotFoundTest'
    reqMethod
    "/wizard-api/questionnaires/f08ead5f-746d-411b-aee6-77ea3d24016a/settings"
    (reqHeadersT [reqAuthHeader])
    (reqBodyT questionnaire1)
    "questionnaire"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
