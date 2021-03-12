module Wizard.Specs.API.Document.List_POST
  ( list_POST
  ) where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import LensesConfig hiding (request)
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Constant.Template
import Shared.Database.DAO.Template.TemplateDAO
import Shared.Database.Migration.Development.Template.Data.Templates
import Shared.Localization.Messages.Public
import Shared.Model.Common.Lens
import Shared.Model.Error.Error
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentCreateJM ()
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Document.DocumentJM ()
import Wizard.Database.DAO.Document.DocumentDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.Migration.Development.Document.Data.Documents
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN_Migration
import qualified Wizard.Database.Migration.Development.Template.TemplateMigration as TML_Migration
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Document.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /documents
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /documents" $ do
    test_201 appContext
    test_400 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/documents"

reqHeadersT authHeader = reqCtHeader : authHeader

reqDtoT qtn =
  DocumentCreateDTO
    { _documentCreateDTOName = "Document"
    , _documentCreateDTOQuestionnaireUuid = qtn ^. uuid
    , _documentCreateDTOQuestionnaireEventUuid = Just $ last (qtn ^. events) ^. uuid'
    , _documentCreateDTOTemplateId = doc1 ^. templateId
    , _documentCreateDTOFormatUuid = doc1 ^. formatUuid
    }

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext = do
  create_test_201 "HTTP 201 CREATED (Owner, Private)" appContext questionnaire1 [reqAuthHeader]
  create_test_201 "HTTP 201 CREATED (Non-Owner, VisibleEdit)" appContext questionnaire3 [reqNonAdminAuthHeader]
  create_test_201 "HTTP 201 CREATED (Anonymous, Public, Sharing)" appContext questionnaire10 []

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
    runInContextIO QTN_Migration.runMigration appContext
    runInContextIO (insertQuestionnaire qtn) appContext
    runInContextIO TML_Migration.runMigration appContext
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
          _ERROR_VALIDATION__TEMPLATE_UNSUPPORTED_VERSION (commonWizardTemplate ^. tId) 1 templateMetamodelVersion
    let expBody = encode expDto
      -- AND: Run migrations
    runInContextIO U_Migration.runMigration appContext
    runInContextIO QTN_Migration.runMigration appContext
    runInContextIO TML_Migration.runMigration appContext
    runInContextIO (updateTemplateById (commonWizardTemplate & metamodelVersion .~ 1)) appContext
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
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView)"
    appContext
    questionnaire2
    []
    _ERROR_SERVICE_USER__MISSING_USER
  create_test_403
    "HTTP 403 FORBIDDEN (Anonymous, VisibleView, Sharing)"
    appContext
    questionnaire7
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
    runInContextIO QTN_Migration.runMigration appContext
    runInContextIO (insertQuestionnaire qtn) appContext
    runInContextIO deleteDocuments appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
     -- AND: Find result in DB and compare with expectation state
    assertCountInDB findDocuments appContext 0
