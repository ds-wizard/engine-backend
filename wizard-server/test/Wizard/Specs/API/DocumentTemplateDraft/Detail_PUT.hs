module Wizard.Specs.API.DocumentTemplateDraft.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateChangeJM ()
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftChangeDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDetailJM ()
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.Context.AppContext
import Wizard.Model.DocumentTemplate.DocumentTemplateDraftDetail
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.DocumentTemplate.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/document-template-drafts/{documentTemplateId}
-- ------------------------------------------------------------------------
detail_PUT :: AppContext -> SpecWith ((), Application)
detail_PUT appContext =
  describe "PUT /wizard-api/document-templates-drafts/{documentTemplateId}" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/document-template-drafts/global:questionnaire-report:2.0.0"

reqHeaders = [reqCtHeader, reqAuthHeader]

reqDto = wizardDocumentTemplateDraftChangeDTO

reqBodyT = encode

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK (change name)"
    appContext
    (wizardDocumentTemplateDraftChangeDTO {name = "Some edited name"} :: DocumentTemplateDraftChangeDTO)
    (wizardDocumentTemplateDraft {name = "Some edited name"} :: DocumentTemplate)
  create_test_200
    "HTTP 200 OK (publish)"
    appContext
    (wizardDocumentTemplateDraftChangeDTO {phase = ReleasedDocumentTemplatePhase} :: DocumentTemplateDraftChangeDTO)
    (wizardDocumentTemplateDraft {phase = ReleasedDocumentTemplatePhase} :: DocumentTemplate)
  create_test_200
    "HTTP 200 OK (publish)"
    appContext
    (wizardDocumentTemplateDraftChangeDTO {version = "3.0.0"} :: DocumentTemplateDraftChangeDTO)
    (wizardDocumentTemplateDraft {version = "3.0.0"} :: DocumentTemplate)

create_test_200 title appContext reqDto expDto =
  it title $
    do
      -- GIVEN: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO (insertPackage germanyPackage) appContext
      runInContextIO (insertQuestionnaire questionnaire1) appContext
      runInContextIO (insertDraftData wizardDocumentTemplateDraftData) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders (reqBodyT reqDto)
      -- THEN: Compare response with expectation
      result <- destructResponse' response
      let (status, headers, resDto) = result :: (Int, ResponseHeaders, DocumentTemplateDraftDetail)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareTemplateDtos resDto expDto
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfTemplateInDB appContext wizardDocumentTemplateDeprecated

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] (reqBodyT reqDto)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] (reqBodyT reqDto) "DOC_TML_WRITE_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/document-template-drafts/deab6c38-aeac-4b17-a501-4365a0a70176"
    reqHeaders
    (reqBodyT reqDto)
    "document_template"
    [("id", "deab6c38-aeac-4b17-a501-4365a0a70176"), ("phase", "DraftDocumentTemplatePhase")]
