module Wizard.Specs.API.KnowledgeModelPackage.Detail_DELETE (
  detail_DELETE,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Data.Either
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Model.Error.Error
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration as KnowledgeModelEditor
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import Wizard.Localization.Messages.Public
import Wizard.Model.Context.AppContext
import Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageService

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /wizard-api/knowledge-model-packages/{pkgId}
-- ------------------------------------------------------------------------
detail_DELETE :: AppContext -> SpecWith ((), Application)
detail_DELETE appContext =
  describe "DELETE /wizard-api/knowledge-model-packages/{pkgId}" $ do
    test_204 appContext
    test_400 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = BS.pack $ "/wizard-api/knowledge-model-packages/" ++ netherlandsKmPackageV2.pId

reqHeaders = [reqAuthHeader, reqCtHeader]

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
      -- AND: Run migrations
      runInContextIO KnowledgeModelPackage.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Find a result
      eitherPackage <- runInContextIO (getPackageDetailById netherlandsKmPackageV2.pId True) appContext
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      liftIO $ isLeft eitherPackage `shouldBe` True
      let (Left (NotExistsError _)) = eitherPackage
      -- AND: We have to end with expression (if there is another way, how to do it, please fix it)
      liftIO $ True `shouldBe` True

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext =
  it "HTTP 400 BAD REQUEST when package can't be deleted" $
    -- GIVEN: Prepare request
    do
      let reqUrl = BS.pack $ "/wizard-api/knowledge-model-packages/" ++ netherlandsKmPackage.pId
      -- AND: Prepare expectation
      let expStatus = 400
      let expHeaders = resCorsHeaders
      let expDto =
            UserError $
              _ERROR_SERVICE_PKG__PKG_CANT_BE_DELETED_BECAUSE_IT_IS_USED_BY_SOME_OTHER_ENTITY
                netherlandsKmPackage.pId
                "knowledge model"
      let expBody = encode expDto
      -- AND: Prepare DB
      runInContextIO KnowledgeModelPackage.runMigration appContext
      runInContextIO KnowledgeModelEditor.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Find a result
      eitherPackages <- runInContextIO findPackages appContext
      -- AND: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Compare state in DB with expectation
      liftIO $ isRight eitherPackages `shouldBe` True
      let (Right packages) = eitherPackages
      liftIO $ length packages `shouldBe` 4

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
    "/wizard-api/knowledge-model-packages/global:non-existing-package:1.0.0"
    reqHeaders
    reqBody
    "knowledge_model_package"
    [("id", "global:non-existing-package:1.0.0")]
