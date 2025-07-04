module Wizard.Specs.API.Questionnaire.List_POST_FromTemplate (
  list_POST_fromTemplate,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateFromTemplateDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireCreateJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN_Migration
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Tenant.Config.TenantConfig hiding (request)
import Wizard.Service.Tenant.Config.ConfigService

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/questionnaires?fromTemplate=true
-- ------------------------------------------------------------------------
list_POST_fromTemplate :: AppContext -> SpecWith ((), Application)
list_POST_fromTemplate appContext =
  describe "POST /wizard-api/questionnaires/from-template" $ do
    test_201 appContext
    test_400 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/questionnaires/from-template"

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqDtoT qtnTmlUuid name =
  QuestionnaireCreateFromTemplateDTO
    { name = name
    , questionnaireUuid = qtnTmlUuid
    }

reqBodyT qtnTmlUuid name = encode (reqDtoT qtnTmlUuid name)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT [reqAuthHeader]
      let reqBody = reqBodyT questionnaire1.uuid questionnaire11.name
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = questionnaire11Dto
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration appContext
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO QTN_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, QuestionnaireDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareQuestionnaireCreateFromTemplateDtos resBody expDto
      -- AND: Find a result in DB
      assertCountInDB findQuestionnaires appContext 4

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext =
  it "HTTP 400 BAD REQUEST (questionnaireCreation: CustomQuestionnaireCreation)" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT [reqAuthHeader]
      let reqBody = reqBodyT questionnaire2.uuid questionnaire11.name
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = UserError . _ERROR_SERVICE_COMMON__FEATURE_IS_DISABLED $ "Questionnaire Template"
      let expBody = encode expDto
      -- AND: Change tenantConfig
      (Right tcQuestionnaire) <- runInContextIO getCurrentTenantConfigQuestionnaire appContext
      let tcQuestionnaireUpdated = tcQuestionnaire {questionnaireCreation = CustomQuestionnaireCreation}
      runInContextIO (modifyTenantConfigQuestionnaire tcQuestionnaireUpdated) appContext
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration appContext
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO QTN_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find a result in DB
      assertCountInDB findQuestionnaires appContext 3

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext =
  it "HTTP 403 FORBIDDEN (isTemplate: False)" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT [reqAuthHeader]
      let reqBody = reqBodyT questionnaire2.uuid questionnaire11.name
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Questionnaire Template"
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration appContext
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO QTN_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find a result in DB
      assertCountInDB findQuestionnaires appContext 3
