module Wizard.Specs.API.QuestionnaireAction.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionDTO
import Wizard.Api.Resource.QuestionnaireAction.QuestionnaireActionJM ()
import Wizard.Database.Migration.Development.QuestionnaireAction.Data.QuestionnaireActions
import qualified Wizard.Database.Migration.Development.QuestionnaireAction.QuestionnaireActionMigration as QA_Migration
import Wizard.Model.Context.AppContext
import Wizard.Model.QuestionnaireAction.QuestionnaireAction
import Wizard.Service.QuestionnaireAction.QuestionnaireActionMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.QuestionnaireAction.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/questionnaire-actions/{id}
-- ------------------------------------------------------------------------
detail_PUT :: AppContext -> SpecWith ((), Application)
detail_PUT appContext =
  describe "PUT /wizard-api/questionnaire-actions/{id}" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/questionnaire-actions/global:questionnaire-action-ftp:3.0.0"

reqHeadersT reqAuthHeader = [reqCtHeader, reqAuthHeader]

reqDto = toChangeDTO questionnaireActionFtp3Edited

reqBody = encode reqDto

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = create_test_200 "HTTP 200 OK" appContext reqAuthHeader

create_test_200 title appContext reqAuthHeader =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeaderPlain : resCorsHeadersPlain
      let expDto = toDTO questionnaireActionFtp3Edited
      let expBody = encode expDto
      let expType (a :: QuestionnaireActionDTO) = a
      -- AND: Run migrations
      runInContextIO QA_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, QuestionnaireActionDTO)
      assertResponseWithoutFields expStatus expHeaders expDto expType response ["updatedAt"]
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfQuestionnaireActionInDB appContext questionnaireActionFtp3Edited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "QTN_ACTION_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/questionnaire-actions/deab6c38-aeac-4b17-a501-4365a0a70176"
    (reqHeadersT reqAuthHeader)
    reqBody
    "questionnaire_action"
    [("id", "deab6c38-aeac-4b17-a501-4365a0a70176")]
