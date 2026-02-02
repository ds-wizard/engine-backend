module Wizard.Specs.API.DocumentTemplateDraft.Detail_Documents_Preview_Settings_PUT (
  detail_documents_preview_settings_PUT,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataChangeJM ()
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftDataJM ()
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as DT_Migration
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.DocumentTemplateDraft.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/document-template-drafts/{dtUuid}/documents/preview/settings
-- ------------------------------------------------------------------------
detail_documents_preview_settings_PUT :: AppContext -> SpecWith ((), Application)
detail_documents_preview_settings_PUT appContext =
  describe "PUT /wizard-api/document-template-drafts/{dtUuid}/documents/preview/settings" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = BS.pack $ "/wizard-api/document-template-drafts/" ++ U.toString wizardDocumentTemplateDraft.uuid ++ "/documents/preview/settings"

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
      runInContextIO DT_Migration.runMigration appContext
      runInContextIO (insertPackage germanyKmPackage) appContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) appContext
      runInContextIO (insertProject project1) appContext
      runInContextIO (insertProject project2) appContext
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
    "/wizard-api/document-template-drafts/3db4265e-8ba2-433d-97fb-6cc504866bbd/documents/preview/settings"
    (reqHeadersT reqAuthHeader)
    reqBody
    "document_template_draft_data"
    [("document_template_uuid", "3db4265e-8ba2-433d-97fb-6cc504866bbd")]
