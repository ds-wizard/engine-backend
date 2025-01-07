module Wizard.Specs.API.DocumentTemplateDraft.Folder.List_Delete_POST (
  list_delete_POST,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeJM ()
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFolders
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Model.Context.AppContext
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.DocumentTemplateDraft.Asset.Common
import Wizard.Specs.API.DocumentTemplateDraft.File.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/document-template-drafts/{documentTemplateId}/folders/delete
-- ------------------------------------------------------------------------
list_delete_POST :: AppContext -> SpecWith ((), Application)
list_delete_POST appContext =
  describe "POST /wizard-api/document-template-drafts/{documentTemplateId}/folders/delete" $ do
    test_204 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/document-template-drafts/global:questionnaire-report:1.0.0/folders/delete"

reqHeadersT reqAuthHeader = [reqCtHeader, reqAuthHeader]

reqDto = folderDeleteDto

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 appContext = create_test_204 "HTTP 201 CREATED (user token)" appContext reqAuthHeader

create_test_204 title appContext reqAuthHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 204
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = ""
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertAbsenceOfTemplateFileInDB appContext fileDefaultHtml
      assertExistenceOfTemplateFileInDB appContext fileDefaultCss wizardDocumentTemplate.tId
      assertExistenceOfTemplateAssetInDB appContext assetLogo

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "DOC_TML_WRITE_PERM"
