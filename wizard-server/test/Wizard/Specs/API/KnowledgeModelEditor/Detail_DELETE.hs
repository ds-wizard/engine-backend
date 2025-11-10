module Wizard.Specs.API.KnowledgeModelEditor.Detail_DELETE (
  detail_DELETE,
) where

import Data.Maybe (fromJust)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Wizard.Service.KnowledgeModel.Editor.EditorService

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /wizard-api/knowledge-model-editors/uuid
-- ------------------------------------------------------------------------
detail_DELETE :: AppContext -> SpecWith ((), Application)
detail_DELETE appContext =
  describe "DELETE /wizard-api/knowledge-model-editors/uuid" $ do
    test_204 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = "/wizard-api/knowledge-model-editors/6474b24b-262b-42b1-9451-008e8363f2b6"

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
      -- AND: Run migrations
      runInContextIO
        ( createEditorWithParams
            amsterdamKnowledgeModelEditorList.uuid
            amsterdamKnowledgeModelEditorList.createdAt
            (fromJust appContext.currentUser)
            amsterdamKnowledgeModelEditorCreate
        )
        appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find result in DB and compare with expectation state
      assertCountInDB findKnowledgeModelEditors appContext 0

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "KM_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/knowledge-model-editors/dc9fe65f-748b-47ec-b30c-d255bbac64a0"
    reqHeaders
    reqBody
    "knowledge_model_editor"
    [("uuid", "dc9fe65f-748b-47ec-b30c-d255bbac64a0")]
