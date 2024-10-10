module Wizard.Specs.API.Questionnaire.Detail_Settings_GET (
  detail_settings_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as M
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import qualified Wizard.Database.Migration.Development.User.UserMigration as U
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireDetailSettings
import qualified Wizard.Service.Package.PackageMapper as PM
import Wizard.Service.Questionnaire.QuestionnaireMapper
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import qualified WizardLib.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper as STM
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import qualified WizardLib.KnowledgeModel.Service.Package.PackageMapper as SPM
import WizardLib.Public.Localization.Messages.Public

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/questionnaires/{qtnUuid}/settings
-- ------------------------------------------------------------------------
detail_settings_GET :: AppContext -> SpecWith ((), Application)
detail_settings_GET appContext =
  describe "GET /wizard-api/questionnaires/{qtnUuid}/settings" $ do
    test_200 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT qtnUuid = BS.pack $ "/wizard-api/questionnaires/" ++ U.toString qtnUuid ++ "/settings"

reqHeadersT authHeader = authHeader

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK (Owner, Private)"
    appContext
    questionnaire1
    [reqAuthHeader]
    [qtn1AlbertEditQtnPermDto]
  create_test_200
    "HTTP 200 OK (Non-Owner, VisibleView)"
    appContext
    questionnaire2
    [reqNonAdminAuthHeader]
    [qtn2AlbertEditQtnPermDto]
  create_test_200
    "HTTP 200 OK (Commentator)"
    appContext
    (questionnaire13 {visibility = PrivateQuestionnaire})
    [reqNonAdminAuthHeader]
    [qtn13NikolaCommentQtnPermDto]
  create_test_200
    "HTTP 200 OK (Non-Commentator, VisibleComment)"
    appContext
    questionnaire13
    [reqIsaacAuthTokenHeader]
    [qtn13NikolaCommentQtnPermDto]
  create_test_200
    "HTTP 200 OK (Anonymous, VisibleComment, AnyoneWithLinkComment)"
    appContext
    (questionnaire13 {sharing = AnyoneWithLinkCommentQuestionnaire})
    []
    [qtn13NikolaCommentQtnPermDto]
  create_test_200
    "HTTP 200 OK (Anonymous, VisibleView, Sharing)"
    appContext
    questionnaire7
    []
    [qtn7AlbertEditQtnPermDto]
  create_test_200
    "HTTP 200 OK (Non-Owner, VisibleEdit)"
    appContext
    questionnaire3
    [reqNonAdminAuthHeader]
    []
  create_test_200
    "HTTP 200 OK (Anonymous, Public, Sharing)"
    appContext
    questionnaire10
    []
    []

create_test_200 title appContext qtn authHeader permissions =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT qtn.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Run migrations
      runInContextIO U.runMigration appContext
      runInContextIO TML.runMigration appContext
      runInContextIO (insertPackage germanyPackage) appContext
      runInContextIO (insertQuestionnaire qtn) appContext
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            QuestionnaireDetailSettings
              { uuid = qtn.uuid
              , name = qtn.name
              , description = qtn.description
              , visibility = qtn.visibility
              , sharing = qtn.sharing
              , isTemplate = qtn.isTemplate
              , migrationUuid = Nothing
              , permissions = permissions
              , projectTags = qtn.projectTags
              , packageId = qtn.packageId
              , package = PM.toSimpleDTO' [] [] (SPM.toPackage germanyPackage)
              , knowledgeModelTags = M.elems km1WithQ4.entities.tags
              , documentTemplate = Just . STM.toDTO $ wizardDocumentTemplate
              , documentTemplateState = toQuestionnaireDetailTemplateState (Just wizardDocumentTemplate)
              , documentTemplatePhase = Just wizardDocumentTemplate.phase
              , formatUuid = Just formatJson.uuid
              , selectedQuestionTagUuids = qtn.selectedQuestionTagUuids
              , fileCount = 0
              }
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    appContext
    questionnaire1
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "View Questionnaire")
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView)"
    appContext
    questionnaire2
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, Public)"
    appContext
    questionnaire3
    []
    _ERROR_SERVICE_USER__MISSING_USER

create_test_403 title appContext qtn authHeader errorMessage =
  it title $
    -- GIVEN: Prepare request
    do
      let reqUrl = reqUrlT qtn.uuid
      let reqHeaders = reqHeadersT authHeader
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError errorMessage
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

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/questionnaires/f08ead5f-746d-411b-aee6-77ea3d24016a/settings"
    [reqHeadersT reqAuthHeader]
    reqBody
    "questionnaire"
    [("uuid", "f08ead5f-746d-411b-aee6-77ea3d24016a")]
