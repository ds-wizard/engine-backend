module Specs.API.Questionnaire.Public_GET
  ( public_get
  ) where

import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Api.Resource.Error.ErrorDTO ()
import Database.DAO.PublicQuestionnaire.PublicQuestionnaireDAO
import Database.Migration.Development.Package.Data.Packages
import Database.Migration.Development.PublicQuestionnaire.Data.PublicQuestionnaires
import qualified
       Database.Migration.Development.PublicQuestionnaire.PublicQuestionnaireMigration
       as PUBQTN
import Localization
import Model.Context.AppContext
import Model.Error.Error
import Service.Questionnaire.QuestionnaireMapper

import Specs.API.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /questionnaires/public
-- ------------------------------------------------------------------------
public_get :: AppContext -> SpecWith Application
public_get appContext =
  describe "GET /questionnaires/public" $ do
    test_200 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/questionnaires/public"

reqHeaders = []

reqBody = ""

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_200 appContext =
  it "HTTP 200 OK" $
     -- GIVEN: Prepare expectation
   do
    let expStatus = 200
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = toDetailWithPackageWithEventsDTO publicQuestionnaire netherlandsPackageV2
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO PUBQTN.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext =
  it "HTTP 404 NOT FOUND - Public questionnaire is not set up" $
      -- GIVEN: Prepare expectation
   do
    let expStatus = 404
    let expHeaders = [resCtHeader] ++ resCorsHeaders
    let expDto = NotExistsError _ERROR_SERVICE_PQ__NOT_SET_UP
    let expBody = encode expDto
    -- AND: Delete public questionnaire
    runInContextIO deletePublicQuestionnaires appContext
    -- WHEN: Call APIA
    response <- request reqMethod reqUrl reqHeaders reqBody
    -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
