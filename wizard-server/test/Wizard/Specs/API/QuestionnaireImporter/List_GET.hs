module Wizard.Specs.API.QuestionnaireImporter.List_GET (
  list_GET,
) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.PageMetadata
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterDTO
import Wizard.Api.Resource.QuestionnaireImporter.QuestionnaireImporterJM ()
import Wizard.Database.Migration.Development.QuestionnaireImporter.Data.QuestionnaireImporters
import qualified Wizard.Database.Migration.Development.QuestionnaireImporter.QuestionnaireImporterMigration as QI_Migration
import Wizard.Model.Context.AppContext
import Wizard.Service.QuestionnaireImporter.QuestionnaireImporterMapper

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- GET /questionnaire-importers
-- ------------------------------------------------------------------------
list_GET :: AppContext -> SpecWith ((), Application)
list_GET appContext =
  describe "GET /questionnaire-importers" $ do
    test_200 appContext
    test_401 appContext
    test_403 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/questionnaire-importers"

reqHeadersT reqAuthHeader = [reqAuthHeader]

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext = do
  create_test_200
    "HTTP 200 OK"
    appContext
    "/questionnaire-importers"
    reqAuthHeader
    ( Page
        "questionnaireImporters"
        (PageMetadata 20 3 1 0)
        (fmap toDTO [questionnaireImporterBio3, questionnaireExtImporter1, questionnaireOntoImporter1])
    )
  create_test_200
    "HTTP 200 OK (query 'q')"
    appContext
    "/questionnaire-importers?q=QuestionnaireImporterBio"
    reqAuthHeader
    (Page "questionnaireImporters" (PageMetadata 20 1 1 0) (fmap toDTO [questionnaireImporterBio3]))
  create_test_200
    "HTTP 200 OK (query 'q' for non-existing)"
    appContext
    "/questionnaire-importers?q=Non-existing Questionnaire Report"
    reqAuthHeader
    (Page "questionnaireImporters" (PageMetadata 20 0 0 0) ([] :: [QuestionnaireImporterDTO]))

create_test_200 title appContext reqUrl reqAuthHeader expDto =
  it title $
    -- GIVEN: Prepare request
    do
      let reqHeaders = reqHeadersT reqAuthHeader
      -- AND: Prepare expectation
      let expStatus = 200
      let expHeaders = resCtHeader : resCorsHeaders
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
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_403 appContext = createNoPermissionTest appContext reqMethod reqUrl [] reqBody "QTN_PERM"
