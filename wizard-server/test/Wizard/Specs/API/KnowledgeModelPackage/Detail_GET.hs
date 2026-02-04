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
import Shared.Common.Model.Error.Error
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailDTO
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import qualified Wizard.Database.Migration.Development.Registry.RegistryMigration as R
import Wizard.Model.Context.AppContext
import WizardLib.Public.Localization.Messages.Public

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/knowledge-model-packages/{uuid}
-- ------------------------------------------------------------------------
detail_GET :: AppContext -> SpecWith ((), Application)
detail_GET appContext =
  describe "GET /wizard-api/knowledge-model-packages/{uuid}" $ do
    test_200 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrlT pkgUuid = BS.pack $ "/wizard-api/knowledge-model-packages/" ++ show pkgUuid

reqHeadersT authHeader = authHeader ++ [reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200 "HTTP 200 OK (public - with token)" appContext [reqAuthHeader] globalKmPackageDetailDto
  create_test_200 "HTTP 200 OK (public - without token)" appContext [] globalKmPackageDetailDto
  create_test_200 "HTTP 200 OK (private - with token)" appContext [reqAuthHeader] globalNetherlandsPackageDetailDto

create_test_200 title appContext authHeader expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT authHeader
      let reqUrl = reqUrlT expDto.uuid
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
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
test_403 appContext = do
  createNoPermissionTest appContext reqMethod (reqUrlT netherlandsKmPackage.uuid) [reqCtHeader] reqBody "PM_READ_PERM"
  it "HTTP 403 FORBIDDEN - private" $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT []
      let reqUrl = reqUrlT globalNetherlandsPackageDetailDto.uuid
      -- AND: Prepare expectation
      let expStatus = 403
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = ForbiddenError _ERROR_SERVICE_USER__MISSING_USER
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
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/knowledge-model-packages/23cb2e5b-23e6-4402-b591-61dc74eba9bb"
    (reqHeadersT [reqAuthHeader])
    reqBody
    "knowledge_model_package"
    [("uuid", "23cb2e5b-23e6-4402-b591-61dc74eba9bb")]
