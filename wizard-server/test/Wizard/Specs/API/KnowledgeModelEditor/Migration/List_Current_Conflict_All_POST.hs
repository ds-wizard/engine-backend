module Wizard.Specs.API.KnowledgeModelEditor.Migration.List_Current_Conflict_All_POST (
  list_Current_Conflict_All_POST,
) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Wizard.Api.Resource.KnowledgeModel.Migration.KnowledgeModelMigrationDTO
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Migration.KnowledgeModelMigrations
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.Migration.KnowledgeModelMigration

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.KnowledgeModelEditor.Migration.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/knowledge-model-editors/uuid/migrations/current/conflict/all
-- ------------------------------------------------------------------------
list_Current_Conflict_All_POST :: AppContext -> SpecWith ((), Application)
list_Current_Conflict_All_POST appContext =
  describe "POST /wizard-api/knowledge-model-editors/uuid/migrations/current/conflict/all" $ do
    test_204 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/knowledge-model-editors/6474b24b-262b-42b1-9451-008e8363f2b6/migrations/current/conflict/all"

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
      let expHeaders = resCtHeader : resCorsHeaders
      let expBody = ""
      -- AND: Prepare database
      runMigrationWithFullDB appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertStateOfMigrationInDB appContext knowledgeModelMigrationDTO CompletedKnowledgeModelMigrationState

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "KM_UPGRADE_PERM"

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
