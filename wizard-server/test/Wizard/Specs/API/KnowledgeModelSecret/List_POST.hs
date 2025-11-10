module Wizard.Specs.API.KnowledgeModelSecret.List_POST (
  list_POST,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Api.Resource.KnowledgeModel.Secret.KnowledgeModelSecretChangeDTO
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Secret.KnowledgeModelSecrets
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModel.KnowledgeModelSecret

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.KnowledgeModelSecret.Common

-- ------------------------------------------------------------------------
-- POST /wizard-api/knowledge-model-secrets
-- ------------------------------------------------------------------------
list_POST :: AppContext -> SpecWith ((), Application)
list_POST appContext =
  describe "POST /wizard-api/knowledge-model-secrets" $ do
    test_200 appContext
    test_400 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPost

reqUrl = "/wizard-api/knowledge-model-secrets"

reqHeaders = [reqAuthHeader, reqCtHeader]

reqDto = kmSecret1ChangeDTO

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
    -- GIVEN: Prepare expectation
    do
      let expStatus = 201
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resBody) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelSecret)
      assertResStatus status expStatus
      assertResHeaders headers expHeaders
      compareKnowledgeModelSecretDtos resBody reqDto
      -- AND: Compare state in DB with expectation
      assertExistenceOfKnowledgeModelSecretInDB appContext reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_400 appContext = createInvalidJsonTest reqMethod reqUrl "name"
