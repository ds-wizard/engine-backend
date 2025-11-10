module Registry.Specs.API.KnowledgeModelPackage.Detail_Bundle_GET (
  detail_bundle_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Registry.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailJM ()
import Registry.Database.Migration.Development.Audit.Data.AuditEntries
import Registry.Model.Context.AppContext
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleJM ()
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Bundle.KnowledgeModelBundles
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage

import Registry.Specs.API.Audit.Common
import Registry.Specs.API.Common
import SharedTest.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /knowledge-model-packages/{pkgId}/bundle
-- ------------------------------------------------------------------------
detail_bundle_GET :: AppContext -> SpecWith ((), Application)
detail_bundle_GET appContext =
  describe "GET /knowledge-model-packages/{pkgId}/bundle" $ do
    test_200 appContext
    test_401 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = BS.pack $ "/knowledge-model-packages/" ++ netherlandsKmPackageV2.pId ++ "/bundle"

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
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = netherlandsV2KmBundle
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfAuditEntryInDB appContext getKnowledgeModelBundleAuditEntry

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
    "/knowledge-model-packages/global:non-existing-km-package:1.0.0/bundle"
    reqHeaders
    reqBody
    "knowledge_model_package"
    [("id", "global:non-existing-km-package:1.0.0")]
