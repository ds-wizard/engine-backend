module Wizard.Specs.API.Document.List_POST (
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
import Shared.Common.Model.Common.Lens
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentCreateJM ()
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Document.DocumentJM ()
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.Migration.Development.Document.Data.Documents
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN_Migration
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Document.Document
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Model.Questionnaire.QuestionnaireSimple
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateFormatDTO
import WizardLib.DocumentTemplate.Constant.DocumentTemplate
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Document.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/documents
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /wizard-api/documents" $ do
    test_201 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/documents"

reqHeadersT authHeader = reqCtHeader : authHeader

reqDtoT qtn =
  DocumentCreateDTO
    { name = "Document"
    , questionnaireUuid = qtn.uuid
    , questionnaireEventUuid = Just . getUuid . last $ qtn.events
    , documentTemplateId = doc1.documentTemplateId
    , formatUuid = doc1.formatUuid
    }

reqBodyT = encode . reqDtoT

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext = do
  create_test_201 "HTTP 201 CREATED (Owner, Private)" appContext questionnaire1 [reqAuthHeader]
  create_test_201 "HTTP 201 CREATED (Non-Owner, VisibleEdit)" appContext questionnaire3 [reqNonAdminAuthHeader]

create_test_201 title appContext qtn authHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeader
      let reqDto = reqDtoT qtn
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration appContext
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO QTN_Migration.runMigration appContext
      runInContextIO deleteDocuments appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, DocumentDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareDocumentDtos resBody reqDto
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findDocuments appContext 1
      assertExistenceOfDocumentInDB appContext reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = do
  createInvalidJsonTest reqMethod reqUrl "kmId"
  it "HTTP 400 BAD REQUEST - Invalid metamodel version of template" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT [reqAuthHeader]
      let reqDto = reqDtoT questionnaire1
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            UserError $
              _ERROR_VALIDATION__TEMPLATE_UNSUPPORTED_METAMODEL_VERSION wizardDocumentTemplate.tId 1 documentTemplateMetamodelVersion
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration appContext
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO QTN_Migration.runMigration appContext
      runInContextIO (updateDocumentTemplateById (wizardDocumentTemplate {metamodelVersion = 1})) appContext
      runInContextIO deleteDocuments appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findDocuments appContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] (reqBodyT questionnaire1)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = do
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, Private)"
    appContext
    questionnaire1
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Edit Questionnaire")
  create_test_403
    "HTTP 403 FORBIDDEN (Non-Owner, VisibleView)"
    appContext
    questionnaire2
    [reqNonAdminAuthHeader]
    (_ERROR_VALIDATION__FORBIDDEN "Edit Questionnaire")

create_test_403 title appContext qtn authHeader errorMessage =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeader
      let reqDto = reqDtoT qtn
      let reqBody = encode reqDto
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError errorMessage
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration appContext
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO QTN_Migration.runMigration appContext
      runInContextIO deleteDocuments appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findDocuments appContext 0
