module Wizard.Specs.API.KnowledgeModelPackage.Detail_GET (
  detail_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Coordinate.Util.Coordinate
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import Wizard.Database.Migration.Development.Registry.Data.RegistryOrganizations
import Wizard.Database.Migration.Development.Registry.Data.RegistryPackages
import qualified Wizard.Database.Migration.Development.Registry.RegistryMigration as R
import Wizard.Model.Context.AppContext
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/knowledge-model-packages/{pkgId}
-- ------------------------------------------------------------------------
detail_GET :: AppContext -> SpecWith ((), Application)
detail_GET appContext =
  describe "GET /wizard-api/knowledge-model-packages/{pkgId}" $ do
    test_200 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT pkgId = BS.pack $ "/wizard-api/knowledge-model-packages/" ++ pkgId

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK (with token)" appContext [reqAuthHeader] globalKmPackage.pId
  create_test_200
    "HTTP 200 OK (with token)"
    appContext
    [reqAuthHeader]
    (buildCoordinate globalKmPackage.organizationId globalKmPackage.kmId "latest")
  create_test_200 "HTTP 200 OK (without token)" appContext [] globalKmPackage.pId
  create_test_200
    "HTTP 200 OK latest (without token)"
    appContext
    []
    (buildCoordinate globalKmPackage.organizationId globalKmPackage.kmId "latest")

create_test_200 title appContext authHeader pkgId =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeader
      let reqUrl = reqUrlT pkgId
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto =
            toDetailDTO
              globalKmPackage
              True
              [globalRegistryPackage]
              [globalRegistryOrganization, nlRegistryOrganization]
              ["0.0.1", "1.0.0"]
              (Just $ "https://registry-test.ds-wizard.org/knowledge-models/" ++ globalKmPackage.pId)
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO KnowledgeModelPackage.runMigration appContext
      runInContextIO R.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext =
  createNoPermissionTest appContext reqMethod (reqUrlT netherlandsKmPackage.pId) [reqCtHeader] reqBody "PM_READ_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/knowledge-model-packages/global:non-existing-package:1.0.0"
    (reqHeadersT [reqAuthHeader])
    reqBody
    "knowledge_model_package"
    [("id", "global:non-existing-package:1.0.0")]
