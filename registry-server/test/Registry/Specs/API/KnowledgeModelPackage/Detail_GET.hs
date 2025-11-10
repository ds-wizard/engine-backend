module Registry.Specs.API.KnowledgeModelPackage.Detail_GET (
  detail_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Registry.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailJM ()
import Registry.Model.Context.AppContext
import Registry.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper
import RegistryLib.Database.Migration.Development.Organization.Data.Organizations
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage

import SharedTest.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /knowledge-model-packages/{pkgId}
-- ------------------------------------------------------------------------
detail_GET :: AppContext -> SpecWith ((), Application)
detail_GET appContext =
  describe "GET /knowledge-model-packages/{pkgId}" $ do
    test_200 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = BS.pack $ "/knowledge-model-packages/" ++ netherlandsKmPackageV2.pId

reqHeaders = [reqCtHeader]

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
      let expDto = toDetailDTO netherlandsKmPackageV2 ["1.0.0", "2.0.0"] orgNetherlands
      let expBody = encode expDto
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/knowledge-model-packages/global:non-existing-km-package:1.0.0"
    reqHeaders
    reqBody
    "knowledge_model_package"
    [("id", "global:non-existing-km-package:1.0.0")]
