module Wizard.Specs.API.KnowledgeModelPackage.Detail_Locales_DELETE (
  detail_locales_DELETE,
) where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (isNothing)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelLocaleDAO
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Locale.KnowledgeModelLocales
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.Locale.KnowledgeModelLocale
import WizardLib.Public.Model.User.RolePermission

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /wizard-api/knowledge-model-packages/{uuid}/locales/{localeUuid}
-- ------------------------------------------------------------------------
detail_locales_DELETE :: AppContext -> SpecWith ((), Application)
detail_locales_DELETE appContext =
  describe "DELETE /wizard-api/knowledge-model-packages/{uuid}/locales/{localeUuid}" $ do
    test_204 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = BS.pack $ "/wizard-api/knowledge-model-packages/" ++ show globalKmPackage.uuid ++ "/locales/" ++ show czechGlobalKmLocale.uuid

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_204 appContext =
  it "HTTP 204 NO CONTENT" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 204
      let expHeaders = resCorsHeaders
      let expBody = ""
      -- AND: Seed locale
      runInContextIO (insertKnowledgeModelLocale czechGlobalKmLocale) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      eLocaleFromDb <- runInContextIO (findKnowledgeModelLocaleByPackageUuidAndCode' globalKmPackage.uuid "cs") appContext
      liftIO $ fmap isNothing eLocaleFromDb `shouldBe` Right True

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
    (BS.pack $ "/wizard-api/knowledge-model-packages/" ++ show globalKmPackage.uuid ++ "/locales/78d1ee0c-2df9-49ec-8f74-8fedf7a6c85e")
    reqHeaders
    reqBody
    "knowledge_model_locale"
    [("knowledge_model_package_uuid", show globalKmPackage.uuid), ("uuid", "78d1ee0c-2df9-49ec-8f74-8fedf7a6c85e")]
