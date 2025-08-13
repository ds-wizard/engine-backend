module Wizard.Specs.API.TypeHint.Test_POST (
  test_POST,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import qualified Wizard.Database.Migration.Development.Branch.BranchMigration as B
import qualified Wizard.Database.Migration.Development.Package.PackageMigration as PKG
import Wizard.Database.Migration.Development.TypeHint.Data.TypeHints
import Wizard.Model.Context.AppContext
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Integrations

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/type-hints/test
-- ------------------------------------------------------------------------
test_POST :: AppContext -> SpecWith ((), Application)
test_POST appContext = describe "POST /wizard-api/type-hints/test" $ do
  test_200 appContext
  test_401 appContext
  test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/type-hints/test"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = typeHintTestRequest

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeader : resCorsHeaders
    let expDto = repositoryApiTypeHintExchange1
    let expBody = encode expDto
    -- AND: Run migrations
    runInContextIO PKG.runMigration appContext
    runInContextIO B.runMigration appContext
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
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "KM_PERM"
