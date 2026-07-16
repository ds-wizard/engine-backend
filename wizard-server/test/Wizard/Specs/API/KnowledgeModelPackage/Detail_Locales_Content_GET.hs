module Wizard.Specs.API.KnowledgeModelPackage.Detail_Locales_Content_GET (
  detail_locales_content_GET,
) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
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
import Wizard.S3.KnowledgeModel.KnowledgeModelLocaleS3

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/knowledge-model-packages/{uuid}/locales/{localeUuid}/content
-- ------------------------------------------------------------------------
detail_locales_content_GET :: AppContext -> SpecWith ((), Application)
detail_locales_content_GET appContext =
  describe "GET /wizard-api/knowledge-model-packages/{uuid}/locales/{localeUuid}/content" $ do
    test_200 appContext
    test_401 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = BS.pack $ "/wizard-api/knowledge-model-packages/" ++ show globalKmPackage.uuid ++ "/locales/" ++ show czechGlobalKmLocale.uuid ++ "/content"

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
      let expHeaders = resCorsHeaders
      let expBody = BSL.fromStrict czechPoContent
      -- AND: Seed locale
      runInContextIO (insertKnowledgeModelLocale czechGlobalKmLocale) appContext
      runInContextIO (putKnowledgeModelLocale czechGlobalKmLocale.uuid translationPoFileName czechPoContent) appContext
      runInContextIO (putKnowledgeModelLocale czechGlobalKmLocale.uuid translationJsonFileName czechJsonContent) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    (BS.pack $ "/wizard-api/knowledge-model-packages/" ++ show globalKmPackage.uuid ++ "/locales/78d1ee0c-2df9-49ec-8f74-8fedf7a6c85e/content")
    reqHeaders
    reqBody
    "knowledge_model_locale"
    [("knowledge_model_package_uuid", show globalKmPackage.uuid), ("uuid", "78d1ee0c-2df9-49ec-8f74-8fedf7a6c85e")]
