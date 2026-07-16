module Wizard.Specs.API.KnowledgeModelPackage.Detail_Locales_POST (
  detail_locales_POST,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe (isJust)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Model.Error.Error
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Localization.Messages.Public
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Api.Resource.KnowledgeModel.Locale.KnowledgeModelLocaleListJM ()
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelLocaleDAO
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Locale.KnowledgeModelLocales
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.Locale.KnowledgeModelLocaleList
import WizardLib.Public.Model.User.RolePermission

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/knowledge-model-packages/{uuid}/locales
-- ------------------------------------------------------------------------
detail_locales_POST :: AppContext -> SpecWith ((), Application)
detail_locales_POST appContext =
  describe "POST /wizard-api/knowledge-model-packages/{uuid}/locales" $ do
    test_200 appContext
    test_400_missing_language appContext
    test_400_duplicate_code appContext
    test_400_invalid_json_content appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = BS.pack $ "/wizard-api/knowledge-model-packages/" ++ show globalKmPackage.uuid ++ "/locales"

boundary = "X-TEST-BOUNDARY"

reqCtMultipartHeader = ("Content-Type", BS.pack $ "multipart/form-data; boundary=" ++ boundary)

reqHeaders = [reqAuthHeader, reqCtMultipartHeader]

reqBody = createMultipartBody czechPoContent czechJsonContent

createMultipartBody :: BS.ByteString -> BS.ByteString -> BSL.ByteString
createMultipartBody poContent jsonContent =
  BSL.fromStrict . BS.concat $
    [ BS.pack $ "--" ++ boundary ++ "\r\n"
    , BS.pack "Content-Disposition: form-data; name=\"name\"\r\n\r\n"
    , BS.pack "Czech\r\n"
    , BS.pack $ "--" ++ boundary ++ "\r\n"
    , BS.pack "Content-Disposition: form-data; name=\"poContent\"; filename=\"translation.po\"\r\n"
    , BS.pack "Content-Type: application/octet-stream\r\n\r\n"
    , poContent
    , BS.pack "\r\n"
    , BS.pack $ "--" ++ boundary ++ "\r\n"
    , BS.pack "Content-Disposition: form-data; name=\"jsonContent\"; filename=\"translation.json\"\r\n"
    , BS.pack "Content-Type: application/json\r\n\r\n"
    , jsonContent
    , BS.pack "\r\n"
    , BS.pack $ "--" ++ boundary ++ "--\r\n"
    ]

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
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelLocaleList)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      liftIO $ resBody.name `shouldBe` "Czech"
      liftIO $ resBody.code `shouldBe` "cs"
      -- AND: Find result in DB and compare with expectation state
      eLocaleFromDb <- runInContextIO (findKnowledgeModelLocaleByPackageUuidAndCode' globalKmPackage.uuid "cs") appContext
      liftIO $ fmap isJust eLocaleFromDb `shouldBe` Right True

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_missing_language appContext =
  it "HTTP 400 BAD REQUEST when PO file has no Language header" $ do
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = UserError _ERROR_VALIDATION__KM_LOCALE_MISSING_LANGUAGE
    let expBody = encode expDto
    response <- request reqMethod reqUrl reqHeaders (createMultipartBody poContentWithoutLanguage czechJsonContent)
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_duplicate_code appContext =
  it "HTTP 400 BAD REQUEST when locale for the language already exists" $ do
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = UserError $ _ERROR_VALIDATION__KM_LOCALE_CODE_UNIQUENESS "cs"
    let expBody = encode expDto
    -- AND: Seed locale with the same code
    runInContextIO (insertKnowledgeModelLocale czechGlobalKmLocale) appContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400_invalid_json_content appContext =
  it "HTTP 400 BAD REQUEST when JSON translation file is invalid" $ do
    let expStatus = 400
    let expHeaders = resCtHeader : resCorsHeaders
    response <- request reqMethod reqUrl reqHeaders (createMultipartBody czechPoContent (BS.pack "{invalid"))
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = MatchBody (\_ _ -> Nothing)}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtMultipartHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtMultipartHeader] reqBody _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/knowledge-model-packages/78d1ee0c-2df9-49ec-8f74-8fedf7a6c85e/locales"
    reqHeaders
    reqBody
    "knowledge_model_package"
    [("uuid", "78d1ee0c-2df9-49ec-8f74-8fedf7a6c85e")]
