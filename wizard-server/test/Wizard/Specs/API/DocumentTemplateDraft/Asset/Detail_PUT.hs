module Wizard.Specs.API.DocumentTemplateDraft.Asset.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()
import Wizard.Api.Resource.DocumentTemplate.Asset.DocumentTemplateAssetChangeJM ()
import Wizard.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.DocumentTemplateDraft.Asset.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/document-template-drafts/{documentTemplateId}/assets/{assetUuid}
-- ------------------------------------------------------------------------
detail_PUT :: AppContext -> SpecWith ((), Application)
detail_PUT appContext =
  describe "PUT /wizard-api/document-template-drafts/{documentTemplateId}/assets/{assetUuid}" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/document-template-drafts/global:questionnaire-report:1.0.0/assets/6c367648-9b60-4307-93b2-0851938adee0"

reqHeaders = [reqCtHeader, reqAuthHeader]

reqDto = assetLogoChangeDTO

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    let expDto = assetLogoEdited
    -- AND: Run migrations
    runInContextIO TML_Migration.runMigration appContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, DocumentTemplateAsset)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareDtos resDto expDto
    -- AND: Find result in DB and compare with expectation state
    assertExistenceOfTemplateAssetInDB appContext assetLogoEdited

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
    "/wizard-api/document-template-drafts/deab6c38-aeac-4b17-a501-4365a0a70176/assets/deab6c38-aeac-4b17-a501-4365a0a70176"
    reqHeaders
    reqBody
    "document_template_asset"
    [("uuid", "deab6c38-aeac-4b17-a501-4365a0a70176")]
