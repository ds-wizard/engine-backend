module Wizard.Specs.API.DocumentTemplateDraft.Detail_Documents_Preview_Settings_PUT (
  detail_documents_preview_settings_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeJM ()
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataJM ()
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Model.Context.AppContext
import WizardLib.KnowledgeModel.Database.DAO.Package.PackageDAO
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.DocumentTemplateDraft.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/document-template-drafts/{documentTemplateId}/documents/preview/settings
-- ------------------------------------------------------------------------
detail_documents_preview_settings_PUT :: AppContext -> SpecWith ((), Application)
detail_documents_preview_settings_PUT appContext =
  describe "PUT /wizard-api/document-template-drafts/{documentTemplateId}/documents/preview/settings" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/document-template-drafts/global:questionnaire-report:2.0.0/documents/preview/settings"

reqHeadersT reqAuthHeader = [reqCtHeader, reqAuthHeader]

reqDto = wizardDocumentTemplateDraftDataChangeDTO

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = create_test_200 "HTTP 200 OK" appContext reqAuthHeader

create_test_200 title appContext reqAuthHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = wizardDocumentTemplateDraftDataDTO
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO (insertPackage germanyPackage) appContext
      runInContextIO (insertQuestionnaire questionnaire1) appContext
      runInContextIO (insertQuestionnaire questionnaire2) appContext
      runInContextIO (insertDraftData wizardDocumentTemplateDraftData) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, DocumentTemplateDraftDataDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto `shouldBe` expDto
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfDraftDataInDB appContext wizardDocumentTemplateDraftDataEdited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "DOC_TML_WRITE_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/document-template-drafts/deab6c38-aeac-4b17-a501-4365a0a70176/documents/preview/settings"
    (reqHeadersT reqAuthHeader)
    reqBody
    "document_template_draft_data"
    [("document_template_id", "deab6c38-aeac-4b17-a501-4365a0a70176")]
