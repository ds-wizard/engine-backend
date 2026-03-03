module Wizard.Specs.API.DocumentTemplateDraft.Asset.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateAssets
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
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
-- PUT /wizard-api/document-template-drafts/{dtUuid}/assets/{assetUuid}
-- ------------------------------------------------------------------------
detail_PUT :: AppContext -> SpecWith ((), Application)
detail_PUT appContext =
  describe "PUT /wizard-api/document-template-drafts/{dtUuid}/assets/{assetUuid}" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = BS.pack $ "/wizard-api/document-template-drafts/" ++ U.toString wizardDocumentTemplate.uuid ++ "/assets/" ++ U.toString assetLogo.uuid

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
    (BS.pack $ "/wizard-api/document-template-drafts/" ++ U.toString wizardDocumentTemplate.uuid ++ "/assets/b918fa4a-b0cf-4bef-ada6-e8cb3848cc75")
    reqHeaders
    reqBody
    "document_template_asset"
    [("uuid", "b918fa4a-b0cf-4bef-ada6-e8cb3848cc75")]
