module Wizard.Specs.API.DocumentTemplate.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import Wizard.Api.Resource.DocumentTemplate.DocumentTemplateSimpleDTO
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import qualified Wizard.Database.Migration.Development.Registry.RegistryMigration as R_Migration
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/document-templates
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext =
  describe "GET /wizard-api/document-templates" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/document-templates"

reqHeadersT reqAuthHeader = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK"
    appContext
    "/wizard-api/document-templates"
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 1 1 0) [wizardDocumentTemplateSimpleDTO])
  create_test_200
    "HTTP 200 OK (query 'q')"
    appContext
    "/wizard-api/document-templates?q=Questionnaire Report"
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 1 1 0) [wizardDocumentTemplateSimpleDTO])
  create_test_200
    "HTTP 200 OK (query 'q' for non-existing)"
    appContext
    "/wizard-api/document-templates?q=Non-existing Questionnaire Report"
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 0 0 0) ([] :: [DocumentTemplateSimpleDTO]))
  create_test_200
    "HTTP 200 OK (query 'templateId')"
    appContext
    "/wizard-api/document-templates?templateId=questionnaire-report"
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 1 1 0) [wizardDocumentTemplateSimpleDTO])
  create_test_200
    "HTTP 200 OK (query 'templateId' for non-existing)"
    appContext
    "/wizard-api/document-templates?templateId=non-existing-template"
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 0 0 0) ([] :: [DocumentTemplateSimpleDTO]))
  create_test_200
    "HTTP 200 OK (outdated=false)"
    appContext
    "/wizard-api/document-templates?outdated=false"
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 1 1 0) [wizardDocumentTemplateSimpleDTO])
  create_test_200
    "HTTP 200 OK (outdated=true)"
    appContext
    "/wizard-api/document-templates?outdated=true"
    reqAuthHeader
    (Page "documentTemplates" (PageMetadata 20 0 0 0) ([] :: [DocumentTemplateSimpleDTO]))

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
      runInContextIO TML_Migration.runMigration appContext
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
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [] reqBody "DOC_TML_READ_PERM"
