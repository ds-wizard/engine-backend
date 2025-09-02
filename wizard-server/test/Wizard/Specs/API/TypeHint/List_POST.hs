module Wizard.Specs.API.TypeHint.List_POST (
  list_POST,
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
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import qualified Wizard.Database.Migration.Development.Branch.BranchMigration as B
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import qualified Wizard.Database.Migration.Development.Package.PackageMigration as PKG
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import Wizard.Database.Migration.Development.TypeHint.Data.TypeHints
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import WizardLib.Public.Localization.Messages.Public

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/type-hints
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /wizard-api/type-hints" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/type-hints"

reqHeadersT authHeader = reqCtHeader : authHeader

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK (Questionnaire, Owner)" appContext questionnaireTypeHintRequest questionnaire15 [reqAuthHeader]
  create_test_200 "HTTP 200 OK (Questionnaire, Editor)" appContext questionnaireTypeHintRequest questionnaire15 [reqNonAdminAuthHeader]
  create_test_200 "HTTP 200 OK (Questionnaire, Anonymous)" appContext questionnaireTypeHintRequest questionnaire15AnonymousEdit []
  create_test_200 "HTTP 200 OK (Branch-Integration)" appContext branchIntegrationTypeHintRequest questionnaire15 [reqAuthHeader]
  create_test_200 "HTTP 200 OK (Branch-Question)" appContext branchQuestionTypeHintRequest questionnaire15 [reqAuthHeader]

create_test_200 title appContext reqDto qtn authHeader =
  it title $ do
    -- GIVEN: Prepare request
    let reqBody = encode reqDto
    let reqHeaders = reqHeadersT authHeader
    -- AND: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = [forestDatasetTypeHint, genomicDatasetTypeHint, animalsDatasetTypeHint]
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO U.runMigration appContext
    runInContextIO TML.runMigration appContext
    runInContextIO PKG.runMigration appContext
    runInContextIO QTN.runMigration appContext
    runInContextIO B.runMigration appContext
    runInContextIO (insertQuestionnaire qtn) appContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = do
  create_test_401 appContext branchIntegrationTypeHintRequest
  create_test_401 appContext branchQuestionTypeHintRequest

create_test_401 appContext reqDto =
  createAuthTest reqMethod reqUrl [reqCtHeader] (encode reqDto)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  create_test_403_questionnaire
    "HTTP 403 FORBIDDEN (Questionnaire, Non-Owner)"
    appContext
    questionnaireTypeHintRequest
    questionnaire15NoPerms
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Edit Questionnaire")
  create_test_403_questionnaire
    "HTTP 403 FORBIDDEN (Questionnaire, Viewer)"
    appContext
    questionnaireTypeHintRequest
    questionnaire15
    [reqIsaacAuthTokenHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Edit Questionnaire")
  create_test_403_questionnaire
    "HTTP 403 FORBIDDEN (Questionnaire, Anonymous)"
    appContext
    questionnaireTypeHintRequest
    questionnaire15
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403_questionnaire
    "HTTP 403 FORBIDDEN (Questionnaire, Anonymous Commenter)"
    appContext
    questionnaireTypeHintRequest
    questionnaire15AnonymousComment
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403_branch appContext branchIntegrationTypeHintRequest
  create_test_403_branch appContext branchQuestionTypeHintRequest

create_test_403_questionnaire title appContext reqDto qtn authHeader reason =
  it title $ do
    -- GIVEN: Prepare request
    let reqBody = encode reqDto
    let reqHeaders = reqHeadersT authHeader
    -- AND: Prepare expectation
    let expStatus = 403
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = ForbiddenError reason
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO U.runMigration appContext
    runInContextIO TML.runMigration appContext
    runInContextIO PKG.runMigration appContext
    runInContextIO QTN.runMigration appContext
    runInContextIO (insertQuestionnaire qtn) appContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

create_test_403_branch appContext reqDto =
  createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] (encode reqDto) "KM_PERM"
