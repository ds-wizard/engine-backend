module Wizard.Specs.API.KnowledgeModelPackage.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageChangeDTO
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as TML_Migration
import Wizard.Model.Context.AppContext
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.KnowledgeModelPackage.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/knowledge-model-packages/{pkgId}
-- ------------------------------------------------------------------------
detail_PUT :: AppContext -> SpecWith ((), Application)
detail_PUT appContext =
  describe "PUT /wizard-api/knowledge-model-packages/{pkgId}" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/knowledge-model-packages/global:core:1.0.0"

reqHeaders = [reqCtHeader, reqAuthHeader]

reqDto = toChangeDTO globalKmPackageDeprecated

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
    do
      -- GIVEN: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = reqDto
      -- AND: Run migrations
      runInContextIO TML_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      result <- destructResponse' response
      let (status, headers, resDto) = result :: (Int, ResponseHeaders, KnowledgeModelPackageChangeDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resDto `shouldBe` expDto
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfPackageInDB appContext globalKmPackageDeprecated

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "PM_WRITE_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/knowledge-model-packages/deab6c38-aeac-4b17-a501-4365a0a70176"
    reqHeaders
    reqBody
    "knowledge_model_package"
    [("id", "deab6c38-aeac-4b17-a501-4365a0a70176")]
