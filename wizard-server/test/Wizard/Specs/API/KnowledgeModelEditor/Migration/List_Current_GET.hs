module Wizard.Specs.API.KnowledgeModelEditor.Migration.List_Current_GET (
  list_current_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Wizard.Database.Migration.Development.KnowledgeModel.Data.Migration.KnowledgeModelMigrations
import Wizard.Model.Context.AppContext

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.KnowledgeModelEditor.Migration.Common

-- ------------------------------------------------------------------------
-- GET /wizard-api/knowledge-model-editors/{uuid}/migrations/current
-- ------------------------------------------------------------------------
list_current_GET :: AppContext -> SpecWith ((), Application)
list_current_GET appContext =
  describe "GET /wizard-api/knowledge-model-editors/{uuid}/migrations/current" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/wizard-api/knowledge-model-editors/6474b24b-262b-42b1-9451-008e8363f2b6/migrations/current"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = knowledgeModelMigrationDTO
      let expBody = encode expDto
      -- AND: Prepare database
      runMigrationWithFullDB appContext
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
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [] "" "KM_UPGRADE_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    reqUrl
    reqHeaders
    reqBody
    "knowledge_model_migration"
    [("editor_uuid", "6474b24b-262b-42b1-9451-008e8363f2b6")]
