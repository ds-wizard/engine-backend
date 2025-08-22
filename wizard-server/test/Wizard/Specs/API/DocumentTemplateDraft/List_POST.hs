module Wizard.Specs.API.DocumentTemplateDraft.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Api.Resource.Common.EntityCreatedWithIdDTO
import Shared.Common.Api.Resource.Common.EntityCreatedWithIdJM ()
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateDTO
import Wizard.Api.Resource.DocumentTemplate.Draft.DocumentTemplateDraftCreateJM ()
import Wizard.Database.DAO.DocumentTemplate.DocumentTemplateDraftDAO
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateDrafts
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Model.Context.AppContext
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/document-template-drafts
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /wizard-api/document-template-drafts" $ do
    test_201 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/document-template-drafts"

reqHeadersT reqAuthHeader = [reqCtHeader, reqAuthHeader]

reqBodyT = encode

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_201 appContext = do
  create_test_201 "HTTP 201 CREATED (without extending)" appContext reqAuthHeader (wizardDocumentTemplateDraftCreateDTO {basedOn = Nothing} :: DocumentTemplateDraftCreateDTO) wizardDocumentTemplateNlDraft

-- create_test_201 "HTTP 201 CREATED (with extending)" appContext reqAuthHeader wizardDocumentTemplateDraftCreateDTO

create_test_201 title appContext reqAuthHeader reqDto expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      let reqBody = reqBodyT reqDto
      -- AND: Prepare expectation
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, EntityCreatedWithIdDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto.aId `shouldBe` expDto.tId
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findDrafts appContext 2

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] (reqBodyT wizardDocumentTemplateDraftCreateDTO)

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] (reqBodyT wizardDocumentTemplateDraftCreateDTO) "DOC_TML_WRITE_PERM"
