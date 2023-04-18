module Wizard.Specs.API.QuestionnaireImporter.Detail_GET (
  detail_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Wizard.Database.Migration.Development.QuestionnaireImporter.Data.QuestionnaireImporters
import qualified Wizard.Database.Migration.Development.QuestionnaireImporter.QuestionnaireImporterMigration as QI_Migration
import Wizard.Model.Context.AppContext
import Wizard.Service.QuestionnaireImporter.QuestionnaireImporterMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /questionnaire-importers/{qi-id}
-- ------------------------------------------------------------------------
detail_GET :: AppContext -> SpecWith ((), Application)
detail_GET appContext =
  describe "GET /questionnaire-importers/{qi-id}" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/questionnaire-importers/global:questionnaire-importer-bio:3.0.0"

reqHeadersT reqAuthHeader = [reqCtHeader, reqAuthHeader]

reqBody = ""

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
      let expHeaders = resCtHeader : resCorsHeaders
      let expDto = toDTO questionnaireImporterBio3
      let expBody = encode expDto
      -- AND: Run migrations
      runInContextIO QI_Migration.runMigration appContext
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
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [reqCtHeader] reqBody "QTN_PERM"

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/questionnaire-importers/deab6c38-aeac-4b17-a501-4365a0a70176"
    (reqHeadersT reqAuthHeader)
    reqBody
    "questionnaire_importer"
    [("id", "deab6c38-aeac-4b17-a501-4365a0a70176")]
