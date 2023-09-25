module Wizard.Specs.API.DocumentTemplateDraft.File.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Api.Resource.DocumentTemplate.File.DocumentTemplateFileChangeJM ()
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Model.Context.AppContext
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFiles
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.DocumentTemplateDraft.File.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/document-template-drafts
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /wizard-api/document-template-drafts/{documentTemplateId}/files" $ do
    test_201 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/document-template-drafts/global:questionnaire-report:1.0.0/files"

reqHeadersT reqAuthHeader = [reqCtHeader, reqAuthHeader]

reqDto = fileNewFileChangeDTO

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext = create_test_201 "HTTP 201 CREATED (user token)" appContext reqAuthHeader

create_test_201 title appContext reqAuthHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = fileNewFile
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, DocumentTemplateFile)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareTemplateFileDtos resDto expDto
      -- AND: Find result in DB and compare with expectation state
      let templateFileNewFileEdited = fileNewFile {uuid = resDto.uuid} :: DocumentTemplateFile
      assertExistenceOfTemplateFileInDB
        appContext
        templateFileNewFileEdited
        wizardDocumentTemplate.tId

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "DOC_TML_WRITE_PERM"
