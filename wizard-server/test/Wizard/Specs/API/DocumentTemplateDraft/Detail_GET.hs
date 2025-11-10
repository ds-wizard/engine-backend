module Wizard.Specs.API.DocumentTemplateDraft.Detail_GET (
  detail_GET,
) where

import Data.Aeson (encode)
import Data.Foldable (traverse_)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageEventDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDataDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Registry.RegistryMigration as R_Migration
import Wizard.Model.Context.AppContext
import Wizard.Service.DocumentTemplate.Draft.DocumentTemplateDraftMapper
import Wizard.Service.Questionnaire.QuestionnaireMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/document-template-drafts/{documentTemplateId}
-- ------------------------------------------------------------------------
detail_GET :: AppContext -> SpecWith ((), Application)
detail_GET appContext =
  describe "GET /wizard-api/document-template-drafts/{documentTemplateId}" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/document-template-drafts/global:questionnaire-report:2.0.0"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  it "HTTP 200 OK" $
    do
      -- GIVEN: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toDraftDetail wizardDocumentTemplateDraft wizardDocumentTemplateDraftFormats wizardDocumentTemplateDraftData (Just . toSuggestion $ questionnaire1) Nothing
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO (insertPackage germanyKmPackage) appContext
      runInContextIO (traverse_ insertPackageEvent germanyKmPackageEvents) appContext
      runInContextIO (insertQuestionnaire questionnaire1) appContext
      runInContextIO (insertDraftData wizardDocumentTemplateDraftData) appContext
      runInContextIO R_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

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
    "/wizard-api/document-template-drafts/global:questionnaire-report:9.9.9"
    reqHeaders
    reqBody
    "document_template"
    [("id", "global:questionnaire-report:9.9.9"), ("phase", "DraftDocumentTemplatePhase")]
