module Wizard.Specs.API.QuestionnaireImporter.Detail_PUT (
  detail_PUT,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterDTO
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterJM ()
import Wizard.Database.Migration.Development.QuestionnaireImporter.Data.QuestionnaireImporters
import qualified Wizard.Database.Migration.Development.QuestionnaireImporter.QuestionnaireImporterMigration as QI_Migration
import Wizard.Model.Context.AppContext
import Wizard.Model.QuestionnaireImporter.QuestionnaireImporter
import Wizard.Service.QuestionnaireImporter.QuestionnaireImporterMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.QuestionnaireImporter.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- PUT /wizard-api/questionnaire-importers/{qi-id}
-- ------------------------------------------------------------------------
detail_PUT :: AppContext -> SpecWith ((), Application)
detail_PUT appContext =
  describe "PUT /wizard-api/questionnaire-importers/{qi-id}" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodPut

reqUrl = "/wizard-api/questionnaire-importers/global:questionnaire-importer-bio:3.0.0"

reqHeadersT reqAuthHeader = [reqCtHeader, reqAuthHeader]

reqDto = toChangeDTO questionnaireImporterBio3Edited

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
      let expDto = toDTO questionnaireImporterBio3Edited
      let expBody = encode expDto
      let expType (a :: QuestionnaireImporterDTO) = a
      -- AND: Run migrations
      runInContextIO QI_Migration.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let (status, headers, resDto) = destructResponse response :: (Int, ResponseHeaders, QuestionnaireImporterDTO)
      assertResponseWithoutFields expStatus expHeaders expDto expType response ["updatedAt"]
      -- AND: Find result in DB and compare with expectation state
      assertExistenceOfQuestionnaireImporterInDB appContext questionnaireImporterBio3Edited

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [reqCtHeader] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "QTN_IMPORTER_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/questionnaire-importers/deab6c38-aeac-4b17-a501-4365a0a70176"
    (reqHeadersT reqAuthHeader)
    reqBody
    "questionnaire_importer"
    [("id", "deab6c38-aeac-4b17-a501-4365a0a70176")]
