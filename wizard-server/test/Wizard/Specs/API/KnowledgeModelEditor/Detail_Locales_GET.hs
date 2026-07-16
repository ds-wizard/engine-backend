module Wizard.Specs.API.KnowledgeModelEditor.Detail_Locales_GET (
  detail_locales_GET,
) where

import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Wizard.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleListJM ()
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelLocaleDAO
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Locale.KnowledgeModelLocales
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Model.KnowledgeModel.Locale.KnowledgeModelLocaleList

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/knowledge-model-editors/{uuid}/locales
-- ------------------------------------------------------------------------
detail_locales_GET :: AppContext -> SpecWith ((), Application)
detail_locales_GET appContext =
  describe "GET /wizard-api/knowledge-model-editors/{uuid}/locales" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = BS.pack $ "/wizard-api/knowledge-model-editors/" ++ show amsterdamKnowledgeModelEditor.uuid ++ "/locales"

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
      let expDto = [czechNetherlandsKmLocaleList]
      -- AND: Run migrations
      runInContextIO KnowledgeModelPackage.runMigration appContext
      runInContextIO KnowledgeModelEditor.runMigration appContext
      runInContextIO (insertKnowledgeModelLocale czechNetherlandsKmLocale) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, [KnowledgeModelLocaleList])
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resBody `shouldBe` expDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [] reqBody "KnowledgeModelEditorsUseRolePermission"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/knowledge-model-editors/78d1ee0c-2df9-49ec-8f74-8fedf7a6c85e/locales"
    reqHeaders
    reqBody
    "knowledge_model_editor"
    [("uuid", "78d1ee0c-2df9-49ec-8f74-8fedf7a6c85e")]
