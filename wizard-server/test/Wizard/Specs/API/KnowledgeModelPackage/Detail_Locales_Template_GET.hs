module Wizard.Specs.API.KnowledgeModelPackage.Detail_Locales_Template_GET (
  detail_locales_template_GET,
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Model.Context.AppContext
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileDTO
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileJM ()
import WizardLib.Public.Model.User.RolePermission

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/knowledge-model-packages/{uuid}/locales/template
-- ------------------------------------------------------------------------
detail_locales_template_GET :: AppContext -> SpecWith ((), Application)
detail_locales_template_GET appContext =
  describe "GET /wizard-api/knowledge-model-packages/{uuid}/locales/template" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = BS.pack $ "/wizard-api/knowledge-model-packages/" ++ show globalKmPackage.uuid ++ "/locales/template"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, TemporaryFileDTO)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resBody.contentType `shouldBe` "application/octet-stream"
      liftIO $ resBody.url `shouldSatisfy` L.isInfixOf ".pot"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [] reqBody _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/knowledge-model-packages/78d1ee0c-2df9-49ec-8f74-8fedf7a6c85e/locales/template"
    reqHeaders
    reqBody
    "knowledge_model_package"
    [("uuid", "78d1ee0c-2df9-49ec-8f74-8fedf7a6c85e")]
