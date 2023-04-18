module Wizard.Specs.API.DocumentTemplateDraft.File.List_GET (
  list_get,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Model.Context.AppContext
import Wizard.Service.DocumentTemplate.File.DocumentTemplateFileMapper
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /document-template-drafts/{documentTemplateId}/files
-- ------------------------------------------------------------------------
list_get :: AppContext -> SpecWith ((), Application)
list_get appContext =
  describe "GET /document-template-drafts/{documentTemplateId}/files" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/document-template-drafts/global:questionnaire-report:1.0.0/files"

reqHeadersT reqAuthHeader = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK" appContext reqAuthHeader

create_test_200 title appContext reqAuthHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = fmap toList [fileDefaultHtml, fileDefaultCss]
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration appContext
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
