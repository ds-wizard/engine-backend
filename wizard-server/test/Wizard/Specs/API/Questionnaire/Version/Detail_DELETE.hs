module Wizard.Specs.API.Questionnaire.Version.Detail_DELETE (
  detail_DELETE,
) where

import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Shared.Common.Api.Resource.Error.ErrorJM ()
import qualified Wizard.Database.Migration.Development.DocumentTemplate.DocumentTemplateMigration as TML
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import qualified Wizard.Database.Migration.Development.Questionnaire.QuestionnaireMigration as QTN
import Wizard.Model.Context.AppContext
import Wizard.Model.Questionnaire.Questionnaire

import SharedTest.Specs.API.Common
import Wizard.Specs.API.Common
import Wizard.Specs.API.Questionnaire.Common
import Wizard.Specs.Common

-- ------------------------------------------------------------------------
-- DELETE /wizard-api/questionnaires/{qtnUuid}/versions/{vUuid}
-- ------------------------------------------------------------------------
detail_DELETE :: AppContext -> SpecWith ((), Application)
detail_DELETE appContext =
  describe "DELETE /wizard-api/questionnaires/{qtnUuid}/versions/{vUuid}" $ do
    test_204 appContext
    test_401 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodDelete

reqUrl = "/wizard-api/questionnaires/af984a75-56e3-49f8-b16f-d6b99599910a/versions/bd6611c8-ea11-48ab-adaa-3ce51b66aae5"

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
      -- AND: Run migrations
      runInContextIO TML.runMigration appContext
      runInContextIO QTN.runMigration appContext
      -- WHEN: Call API
      response <- request reqMethod reqUrl reqHeaders reqBody
      -- THEN: Compare response with expectation
      let responseMatcher =
            ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
      response `shouldRespondWith` responseMatcher
      -- AND: Find a result in DB
      assertExistenceOfQuestionnaireInDB appContext (questionnaire1 {versions = []})

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_401 appContext = createAuthTest reqMethod reqUrl [] reqBody

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  createNotFoundTest'
    reqMethod
    "/wizard-api/questionnaires/00084a75-56e3-49f8-b16f-d6b99599910a/versions/bd6611c8-ea11-48ab-adaa-3ce51b66aae5"
    reqHeaders
    reqBody
    "questionnaire"
    [("uuid", "00084a75-56e3-49f8-b16f-d6b99599910a")]
