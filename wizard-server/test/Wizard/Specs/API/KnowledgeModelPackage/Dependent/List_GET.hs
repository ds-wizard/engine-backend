module Wizard.Specs.API.KnowledgeModelPackage.Dependent.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (traverse_)
import qualified Data.UUID as U
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackageDependents
import qualified Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelPackageMigration as KnowledgeModelPackage
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Model.Context.AppContext

import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateFormatDAO
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/knowledge-model-packages/{uuid}/dependents
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext =
  describe "GET /wizard-api/knowledge-model-packages/{uuid}/dependents" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = BS.pack $ "/wizard-api/knowledge-model-packages/" ++ U.toString netherlandsKmPackage.uuid ++ "/dependents"

reqHeaders = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  it "HTTP 200 OK" $
    -- GIVEN: Prepare request
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = [netherlandsKmPackageDeletionImpact, netherlandsKmPackageV2DeletionImpact]
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO KnowledgeModelPackage.runMigration appContext
      runInContextIO (insertDocumentTemplate wizardDocumentTemplate) appContext
      runInContextIO (traverse_ insertDocumentTemplateFormat wizardDocumentTemplateFormats) appContext
      runInContextIO (insertProject project4) appContext
      runInContextIO (insertKnowledgeModelEditor amsterdamKnowledgeModelEditor) appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "PM_READ_PERM"
