module Wizard.Specs.API.DocumentTemplate.List_Suggestions_GET (
  list_suggestions_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import qualified Wizard.Database.Migration.Development.Registry.RegistryMigration as R_Migration
import qualified Wizard.Database.Migration.Development.User.UserMigration as U_Migration
import Wizard.Model.Context.AppContext
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSuggestionDTO
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import WizardLib.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /document-templates/suggestions
-- ------------------------------------------------------------------------
list_suggestions_GET :: AppContext -> SpecWith ((), Application)
list_suggestions_GET appContext =
  describe "GET /document-templates/suggestions" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/document-templates/suggestions"

reqHeadersT reqAuthHeader = [reqNonAdminAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK"
    appContext
    "/document-templates/suggestions"
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 1 1 0) [toSuggestionDTO wizardDocumentTemplate])
  create_test_200
    "HTTP 200 OK (query 'q')"
    appContext
    "/document-templates/suggestions?q=Questionnaire Report"
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 1 1 0) [toSuggestionDTO wizardDocumentTemplate])
  create_test_200
    "HTTP 200 OK (query 'pkgId')"
    appContext
    "/document-templates/suggestions?pkgId=global:core:1.0.0"
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 1 1 0) [toSuggestionDTO wizardDocumentTemplate])
  create_test_200
    "HTTP 200 OK (query 'pkgId' - no templates)"
    appContext
    "/document-templates/suggestions?pkgId=org.nl:core-nl:1.0.0"
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 0 0 0) ([] :: [DocumentTemplateSuggestionDTO]))
  create_test_200
    "HTTP 200 OK (query 'q' for non-existing)"
    appContext
    "/document-templates/suggestions?q=Non-existing Questionnaire Report"
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 0 0 0) ([] :: [DocumentTemplateSuggestionDTO]))

create_test_200 title appContext reqUrl reqAuthHeader expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO U_Migration.runMigration appContext
      runInContextIO TML_Migration.runMigration appContext
      runInContextIO R_Migration.runMigration appContext
      runInContextIO (updateDocumentTemplateById $ wizardDocumentTemplate {allowedPackages = [packagePatternAllEdited]}) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [] reqBody "DOC_TML_READ_PERM"
