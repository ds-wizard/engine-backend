module Specs.API.Feedback.Detail_GET
  ( detail_get
  ) where

import Control.Monad.Reader (asks)
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Wai (Application)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)
import Test.Hspec.Wai.Matcher

import Database.Migration.Development.Feedback.Data.Feedbacks
import qualified
       Database.Migration.Development.Feedback.FeedbackMigration as F
import Model.Context.AppContext
import Service.Feedback.FeedbackMapper
import Service.Feedback.FeedbackService

import Specs.API.Common
import Specs.Common

-- ------------------------------------------------------------------------
-- GET /feedbacks/{feedbackUuid}
-- ------------------------------------------------------------------------
detail_get :: AppContext -> SpecWith Application
detail_get appContext =
  describe "GET /feedbacks/{feedbackUuid}" $ do
    test_200 appContext
    test_404 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/feedbacks/c44c06d1-ad9f-4f73-9c05-2aa9eddacae1"

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
    dswConfig <- runInContextIO (asks _appContextAppConfig) appContext
    let iUrl = createIssueUrl dswConfig feedback1
    let expDto = toDTO feedback1 iUrl
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO F.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
test_404 appContext = createNotFoundTest reqMethod "/feedbacks/nonExistingShortUuid" reqHeaders reqBody
