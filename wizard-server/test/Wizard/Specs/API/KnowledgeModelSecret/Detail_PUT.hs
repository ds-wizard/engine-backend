module Wizard.Specs.API.KnowledgeModelSecret.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Api.Resource.KnowledgeModelSecret.KnowledgeModelSecretChangeJM ()
import Wizard.Api.Resource.KnowledgeModelSecret.KnowledgeModelSecretJM ()
import Wizard.Database.Migration.Development.KnowledgeModelSecret.Data.KnowledgeModelSecrets
import qualified Wizard.Database.Migration.Development.KnowledgeModelSecret.KnowledgeModelSecretMigration as KMS_Migration
import Wizard.Model.Context.AppContext
import Wizard.Model.KnowledgeModelSecret.KnowledgeModelSecret

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.KnowledgeModelSecret.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/knowledge-model-secrets/{uuid}
-- ------------------------------------------------------------------------
detail_PUT :: AppContext -> SpecWith ((), Application)
detail_PUT appContext =
  describe "PUT /wizard-api/knowledge-model-secrets/{uuid}" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/knowledge-model-secrets/171635b5-d5e7-4bba-8dd0-93765866aea1"

reqHeaders = [reqCtHeader, reqAuthHeader]

reqDto = kmSecret1ChangeDTO

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $ do
    -- GIVEN: Prepare expectation
    let expStatus = 200
    let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
    let expDto = kmSecret1Edited
    -- AND: Run migrations
    runInContextIO KMS_Migration.runMigration appContext
    -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, KnowledgeModelSecret)
    assertResStatus status expStatus
    assertResHeaders headers expHeaders
    compareKnowledgeModelSecretDtos resDto expDto
    -- AND: Find result in DB and compare with expectation state
    assertExistenceOfKnowledgeModelSecretInDB appContext kmSecret1Edited

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
    "/wizard-api/knowledge-model-secrets/345fbf3b-06d5-4660-a108-fd30deb1a44f"
    reqHeaders
    reqBody
    "knowledge_model_secret"
    [("uuid", "345fbf3b-06d5-4660-a108-fd30deb1a44f")]
