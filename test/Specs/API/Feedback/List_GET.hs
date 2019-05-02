module Specs.API.Feedback.List_GET
  ( list_get
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
-- GET /feedbacks
-- ------------------------------------------------------------------------
list_get :: AppContext -> SpecWith Application
list_get appContext = describe "GET /feedbacks" $ do test_200 appContext

-- ----------------------------------------------------
-- ----------------------------------------------------
-- ----------------------------------------------------
reqMethod = methodGet

reqUrl = "/feedbacks"

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
    let iUrl1 = createIssueUrl dswConfig feedback1
    let iUrl2 = createIssueUrl dswConfig feedback2
    let expDto = [toDTO feedback1 iUrl1, toDTO feedback2 iUrl2]
    let expBody = encode expDto
     -- AND: Run migrations
    runInContextIO F.runMigration appContext
     -- WHEN: Call API
    response <- request reqMethod reqUrl reqHeaders reqBody
     -- THEN: Compare response with expectation
    let responseMatcher =
          ResponseMatcher {matchHeaders = expHeaders, matchStatus = expStatus, matchBody = bodyEquals expBody}
    response `shouldRespondWith` responseMatcher
