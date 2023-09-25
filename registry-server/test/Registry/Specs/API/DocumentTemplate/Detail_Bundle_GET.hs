module Registry.Specs.API.DocumentTemplate.Detail_Bundle_GET (
  detail_bundle_GET,
) where

import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import qualified Registry.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML_Migration
import Registry.Model.Context.AppContext
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

import Registry.Specs.API.Common
import Registry.Specs.Common
import SharedTest.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /document-templates/{documentTemplateId}/bundle
-- ------------------------------------------------------------------------
detail_bundle_GET :: AppContext -> SpecWith ((), Application)
detail_bundle_GET appContext =
  describe "GET /document-templates/{documentTemplateId}/bundle" $ do
    test_200 appContext
    test_401 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = BS.pack $ "/document-templates/" ++ wizardDocumentTemplate.tId ++ "/bundle"

reqHeaders = [reqAdminAuthHeader, reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCorsHeadersPlain
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, String)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/document-templates/global:non-existing-template:1.0.0/bundle"
    reqHeaders
    reqBody
    "document_template"
    [("id", "global:non-existing-template:1.0.0")]
